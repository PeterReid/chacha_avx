extern crate rassembler;

use rassembler::parser::Arg;
use rassembler::Assembler;
use rassembler::{HWord, OWord};
use rassembler::HWord::*;
use rassembler::OWord::*;
use rassembler::QWord::*;
use rassembler::{rip_relative, rip_nonrelative};

use std::collections::HashSet;
use std::hash::Hash;
use std::fmt::Debug;

fn no_arg() -> Option<Arg> {
    None
}

#[derive(Debug, Copy, Clone)]
enum Op<R: Copy> {
    Add{dest: R, source: R},
    Xor{dest: R, source: R},
    Roll16{dest: R},
    ShiftLeftIntoScratch{source: R, amount: u8},
    ShiftSelfRight{dest: R, amount: u8},
    CombineWithScratch{dest: R},
    Scratchify{will_be_scratch: R},
    CommitNewScratch,
}

impl<R: Copy+PartialEq+Eq> Op<R> {
    fn reads(self) -> Vec<R> {
        match self {
            Op::Add{dest, source} => vec![source, dest],
            Op::Xor{dest, source} => vec![source, dest],
            Op::Roll16{dest} => vec![dest],
            Op::ShiftLeftIntoScratch{source, amount:_} => vec![source],
            Op::ShiftSelfRight{dest, amount:_} =>  vec![dest],
            Op::CombineWithScratch{dest} => vec![dest],
            Op::Scratchify{will_be_scratch:_} => vec![],
            Op::CommitNewScratch => vec![],
        } 
    }
    fn writes(self) -> Vec<R> {
        match self {
            Op::Add{dest, source:_} => vec![dest],
            Op::Xor{dest, source:_} => vec![dest],
            Op::Roll16{dest} => vec![dest],
            Op::ShiftLeftIntoScratch{source:_, amount:_} => vec![],
            Op::ShiftSelfRight{dest, amount:_} => vec![dest],
            Op::CombineWithScratch{dest} => vec![dest],
            Op::Scratchify{will_be_scratch:_} => vec![],
            Op::CommitNewScratch => vec![],
        } 
    }

    fn requires(self, x: R) -> bool {
        self.reads().contains(&x) || self.writes().contains(&x)
    }
}
/// x += y; z ^= x; z <<= roll_amount;

trait InstrGen where Self: std::marker::Sized {
    fn regs() -> [[Self; 4]; 4];
    fn reg_byte_size() -> i32;
}

fn opset<R: Copy+Eq+PartialEq>(ops: &mut Vec<Op<R>>, x: R, y: R, z: R, roll_amount: u8) {
    ops.push(Op::Add{dest: x, source: y});
    ops.push(Op::Xor{dest: z, source: x});
    if roll_amount == 16 {
        ops.push(Op::Roll16{dest: z});
    } else {
        ops.push(Op::ShiftLeftIntoScratch{source: z, amount: roll_amount});
        ops.push(Op::ShiftSelfRight{dest: z, amount: 32 - roll_amount});
        ops.push(Op::CombineWithScratch{dest: z});
    }
}

fn quarter_round<R: Copy+PartialEq+Eq>(ops: &mut Vec<Op<R>>, a: R, b: R, c: R, d: R) {
    opset(ops, a, b, d, 16); // a += b; d ^= a; d <<<= 16;
    opset(ops, c, d, b, 12); // c += d; b ^= c; b <<<= 12;
    opset(ops, a, b, d, 8); // a += b; d ^= a; d <<<= 8;
    opset(ops, c, d, b, 7); // c += d; b ^= c; b <<<= 7;
}

fn print_ops<R: Copy+PartialEq+Eq+::std::fmt::Debug>(ops: &[Op<R>]) {
    println!("[");
    for (idx, op) in ops.iter().enumerate() {
        println!(" {}.  {:?}", idx, op);
    }
    println!("]");
}

// Rearrange instructions so that the first one that require()s `defer` is as far forward
// as possible.
fn defer_usage_of<R: Copy+PartialEq+Eq+Hash>(defer: R, ops: &[Op<R>], starting_at: usize) -> (Vec<Op<R>>, usize) {
    let mut affected = HashSet::new();
    affected.insert(defer);
    
    let mut rearranged = ops[0..starting_at].to_vec();
    let mut deferred = Vec::new();
    
    for op in ops[starting_at..].iter() {
        let must_go_later = op.requires(defer) || affected.iter().any(|x| op.reads().contains(x));
        //println!("{}: must_go_later = {}", idx, must_go_later);
        if must_go_later {
            for also_affected in op.writes() {
                affected.insert(also_affected);
            }
            deferred.push(*op);
        } else {
            rearranged.push(*op);
        }
    }
    
    let deferred_until_after = rearranged.len();
    rearranged.extend(deferred);
    
    (rearranged, deferred_until_after)
}

fn find_first_usage_before<R: Copy+PartialEq+Eq>(ops: &[Op<R>], reg: R, mut index: usize) -> usize {
    index -= 1;
    while index > 0 && !ops[index].requires(reg) {
        index -= 1;
    }
    index
}

fn insert_scratch_transition<R: Copy+PartialEq+Eq>(_old_scratch: R, new_scratch: R, ops: &mut Vec<Op<R>>, offset: usize) {
    let scratchify_must_be_after = find_first_usage_before(&ops[..], new_scratch, offset);
    assert!(scratchify_must_be_after + 6 < offset);

    ops.insert((scratchify_must_be_after*2 + offset)/3, Op::Scratchify{will_be_scratch: new_scratch});
    ops.insert((scratchify_must_be_after + offset*2)/3, Op::CommitNewScratch);
}

fn generate<R: Copy+PartialEq+Eq+Hash+Into<Arg>+Debug+InstrGen>(/*instr_gen: &InstrGen<R>*/ asm: &mut Assembler) {
    let reg: [[R; 4]; 4] = R::regs();
    println!("{:?}", reg);
    
    let reg_byte_size = R::reg_byte_size();
    
    let incrementing_constants = reg[0][0];
    let low_counters_unincreased = reg[0][1];
    //let zero_holder = reg[0][2];
    let low_counters = reg[3][3];
    let high_counters = reg[3][2];
    asm.movdqa(incrementing_constants, rip_relative("chacha_avx_constant"));
    asm.vbroadcastss(low_counters_unincreased, Rcx.value_at_offset(4 * (4*3 + 3) as i32));
    asm.vbroadcastss(high_counters, Rcx.value_at_offset(4 * (4*3 + 2) as i32));
    
    asm.vpaddd(low_counters, low_counters_unincreased, incrementing_constants);
    // TODO: Handle overflow in the counters
    // Might want to do a 64-bit add...
    // asm.vxorpd(zero_holder, zero_holder, zero_holder); // zero a register, so we can look for overflow
    // asm.pcmpeqd(low_counters, zero_holder);
    // asm.vpaddd(high_counters, zero_holder, high_counters);
    // TODO: Handle overflow in the counters ##################
    
    for row in 0..4 {
        for col in 0..4 {
            if reg[row][col] == low_counters || reg[row][col] == high_counters {
                // Already loaded this
                continue;
            }
            asm.vbroadcastss(reg[row][col], Rcx.value_at_offset(4 * (row*4 + col) as i32));
        }
    }
    
    let mut ops = Vec::new();
    
    for col in 0..4 {
        quarter_round(&mut ops, 
            reg[0][col], 
            reg[1][col], 
            reg[2][col], 
            reg[3][col]);
    }
    for diagonal in 0..4 {
        quarter_round(&mut ops, 
            reg[0][(diagonal+0)%4], 
            reg[1][(diagonal+1)%4], 
            reg[2][(diagonal+2)%4], 
            reg[3][(diagonal+3)%4]);
    }
    
    
    
    let scratch0 = reg[3][0];
    let scratch1 = reg[2][2];
    
    let ops_and_offset = defer_usage_of(scratch0, &ops, 0);
    let mut ops = ops_and_offset.0;
    insert_scratch_transition(scratch0, scratch1, &mut ops, ops_and_offset.1);
    let ops_and_offset = defer_usage_of(scratch1, &ops, ops_and_offset.1 + 2); // +2 because of the insert_scratch_transition instructions
    let mut ops = ops_and_offset.0;
    insert_scratch_transition(scratch1, scratch0, &mut ops, ops_and_offset.1);
    {
        let ops_and_offset_final = defer_usage_of(scratch0, &ops, ops_and_offset.1);
        assert!(ops_and_offset_final.1 == ops.len());
    }
    
    // TODO: Make sure temp variables are stored in properly-aligned memory, then use vmovapd
    
    print_ops(&ops[..]);
    
    let scratch_offsets = (128, 128+reg_byte_size);
    let mut scratch = scratch0;
    let mut scratch_offset = scratch_offsets.0;
    let mut pending_scratch: Option<(R, i32)> = None;
    
    // Set up our initial scratch register.
    asm.vmovupd(Rsp.value_at_offset(scratch_offset), scratch);
    
    asm.local("chacha_avx_top");
    
    for (idx, op) in ops.into_iter().enumerate() {
        println!("Coding op {}: {:?} with scratch = {:?}", idx, op, scratch);
        assert!(!op.requires(scratch));
        if let Some(pending_scratch) = pending_scratch {
            assert!(!op.requires(pending_scratch.0));
        }
        
        match op {
            Op::Add{dest, source} => {
                asm.vpaddd(dest, source, dest);
            }
            Op::Xor{dest, source} => {
                asm.vxorpd(dest, dest, source);
            }
            Op::Roll16{dest} => {
                asm.vpshufb(dest, dest, dest); 
            }
            Op::ShiftLeftIntoScratch{source, amount} => {
                asm.vpslld(scratch, source, amount as i8);
            }
            Op::ShiftSelfRight{dest, amount} => {
                asm.vpsrld(dest, dest, amount as i8);
            }
            Op::CombineWithScratch{dest} => {
                asm.vxorpd(dest, dest, scratch);
            }
            Op::Scratchify{will_be_scratch} => {
                let other_offset = if scratch_offset == scratch_offsets.0 { scratch_offsets.1 } else { scratch_offsets.0 };
                asm.vmovupd(Rsp.value_at_offset(other_offset), will_be_scratch);
                
                pending_scratch = Some((will_be_scratch, other_offset));
            }
            Op::CommitNewScratch{} => {
                asm.vmovupd(scratch, Rsp.value_at_offset(scratch_offset));
                let new_scratch = pending_scratch.take().expect("no pending scratch");
                scratch = new_scratch.0;
                scratch_offset = new_scratch.1;
            }
        }
    }
    
    asm.sub(Rax, 1);
    asm.ja(rip_nonrelative("chacha_avx_top"));
    
    
    // Restore the scratch into a register
    asm.vmovupd(scratch, Rsp.value_at_offset(scratch_offset));
    
    for (row_idx, row) in reg.iter().enumerate() {
        for (col_idx, r) in row.iter().enumerate() {
            asm.vmovupd(Rdx.value_at_offset(((row_idx*4+col_idx) as i32)*reg_byte_size), *r);
        }
    }
    
    asm.ret(no_arg());
    
    asm.align(16);
    asm.local("chacha_avx_constant");
    asm.constant(&[1,0,0,0, 2,0,0,0, 3,0,0,0, 4,0,0,0][..]);
    
}

#[allow(unused)]
fn analyze(ops: Vec<Op<HWord>>) {
    print_ops(&ops[..]);
    
    let ymms = &[ Ymm0, Ymm1, Ymm2, Ymm3, Ymm4, Ymm5, Ymm6, Ymm7, Ymm8, Ymm9, Ymm10, Ymm11, Ymm12, Ymm13, Ymm14, Ymm15 ];
    for reg0 in ymms {
        let ops_and_offset = defer_usage_of(*reg0, &ops, 0);
        let first_scratch_used_until = ops_and_offset.1;
        
        println!("deferred {:?} until {}", reg0, ops_and_offset.1);
        for reg1 in ymms {
            
        
            let ops_and_offset = defer_usage_of(*reg1, &ops_and_offset.0, ops_and_offset.1);
            let second_scratch_used_until = ops_and_offset.1;
            
            // We care about how long before the offset it has been that the new scratch register has been idle.
            // That gives us time to save it into memory.
            let mut previous_use_of_reg1 = first_scratch_used_until;
            while previous_use_of_reg1 > 0 && !ops_and_offset.0[previous_use_of_reg1].requires(*reg1) {
                previous_use_of_reg1 -= 1;
            }
            
            let mut previous_use_of_reg0 = second_scratch_used_until;
            while second_scratch_used_until > 0 && !ops_and_offset.0[previous_use_of_reg0].requires(*reg0) {
                previous_use_of_reg0 -= 1;
            }
            
            let ops_and_offset_final = defer_usage_of(*reg0, &ops_and_offset.0, ops_and_offset.1);
            if ops_and_offset_final.1 != ops.len() {
                continue;
            }
            
            
            
            println!("  deferred {:?} (which we have {} instructions to save) until {} and (then {} with {} instructions to save)", 
                reg1, 
                first_scratch_used_until - previous_use_of_reg1 /* actually this could be first use of scratch later */,
                ops_and_offset.1, 
                ops_and_offset_final.1,
                second_scratch_used_until - previous_use_of_reg0
                );
            
            
        }
        //{ let x = defer_usage_of(Ymm15, &ops, 59); println!("deferred {:?} until {}", reg, x.1); }
    }
}

impl InstrGen for HWord {
    fn regs() -> [[HWord; 4]; 4] {
        [
            [Ymm0, Ymm1, Ymm2, Ymm3],
            [Ymm4, Ymm5, Ymm6, Ymm7],
            [Ymm8, Ymm9, Ymm10, Ymm11],
            [Ymm12, Ymm13, Ymm14, Ymm15],
        ]
    }
    
    fn reg_byte_size() -> i32 {
        32
    }
}

impl InstrGen for OWord {
    fn regs() -> [[OWord; 4]; 4] {
        [
            [Xmm0, Xmm1, Xmm2, Xmm3],
            [Xmm4, Xmm5, Xmm6, Xmm7],
            [Xmm8, Xmm9, Xmm10, Xmm11],
            [Xmm12, Xmm13, Xmm14, Xmm15],
        ]
    }
    
    fn reg_byte_size() -> i32 {
        16
    }
}

fn main() {
    let mut asm = Assembler::new();
    
    //asm.global("chacha_avx2");
    //generate::<HWord>(&mut asm);
    
    asm.global("chacha_avx");
    generate::<OWord>(&mut asm);
    
    asm.output();
    
    
}
