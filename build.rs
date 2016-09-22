extern crate rassembler;

use rassembler::parser::Arg;
use rassembler::Assembler;
use rassembler::HWord;
use rassembler::HWord::*;
use rassembler::OWord::*;
use rassembler::Byte::*;
use rassembler::QWord::*;
use rassembler::DWord::*;

use std::collections::HashSet;

fn no_arg() -> Option<Arg> {
    None
}

#[derive(Debug, Copy, Clone)]
enum Op {
    Add{dest: HWord, source: HWord},
    Xor{dest: HWord, source: HWord},
    Roll16{dest: HWord},
    ShiftLeftIntoScratch{source: HWord, amount: u8},
    ShiftSelfRight{dest: HWord, amount: u8},
    CombineWithScratch{dest: HWord},
    Scratchify{will_be_scratch: HWord},
    CommitNewScratch,
}

impl Op {
    fn reads(self) -> Vec<HWord> {
        match self {
            Op::Add{dest, source} => vec![source, dest],
            Op::Xor{dest, source} => vec![source, dest],
            Op::Roll16{dest} => vec![dest],
            Op::ShiftLeftIntoScratch{source, amount} => vec![source],
            Op::ShiftSelfRight{dest, amount} =>  vec![dest],
            Op::CombineWithScratch{dest} => vec![dest],
            Op::Scratchify{will_be_scratch} => vec![],
            Op::CommitNewScratch => vec![],
        } 
    }
    fn writes(self) -> Vec<HWord> {
        match self {
            Op::Add{dest, source} => vec![dest],
            Op::Xor{dest, source} => vec![dest],
            Op::Roll16{dest} => vec![dest],
            Op::ShiftLeftIntoScratch{source, amount} => vec![],
            Op::ShiftSelfRight{dest, amount} => vec![dest],
            Op::CombineWithScratch{dest} => vec![dest],
            Op::Scratchify{will_be_scratch} => vec![],
            Op::CommitNewScratch => vec![],
        } 
    }

    fn requires(self, x: HWord) -> bool {
        self.reads().contains(&x) || self.writes().contains(&x)
    }
}
/// x += y; z ^= x; z <<= roll_amount;
fn opset(ops: &mut Vec<Op>, x: HWord, y: HWord, z: HWord, roll_amount: u8) {
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

fn quarter_round(ops: &mut Vec<Op>, a: HWord, b: HWord, c: HWord, d: HWord) {
    opset(ops, a, b, d, 16); // a += b; d ^= a; d <<<= 16;
    opset(ops, c, d, b, 12); // c += d; b ^= c; b <<<= 12;
    opset(ops, a, b, d, 8); // a += b; d ^= a; d <<<= 8;
    opset(ops, c, d, b, 7); // c += d; b ^= c; b <<<= 7;
}

fn print_ops(ops: &[Op]) {
    println!("[");
    for (idx, op) in ops.iter().enumerate() {
        println!(" {}.  {:?}", idx, op);
    }
    println!("]");
}

// Rearrange instructions so that the first one that require()s `defer` is as far forward
// as possible.
fn defer_usage_of(defer: HWord, ops: &[Op], starting_at: usize) -> (Vec<Op>, usize) {
    let mut affected = HashSet::new();
    affected.insert(defer);
    
    let mut rearranged = ops[0..starting_at].to_vec();
    let mut deferred = Vec::new();
    
    for (idx, op) in ops[starting_at..].iter().enumerate() {
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

fn find_first_usage_before(ops: &[Op], reg: HWord, mut index: usize) -> usize {
    index -= 1;
    while index > 0 && !ops[index].requires(reg) {
        index -= 1;
    }
    index
}

fn insert_scratch_transition(old_scratch: HWord, new_scratch: HWord, ops: &mut Vec<Op>, offset: usize) {
    let scratchify_must_be_after = find_first_usage_before(&ops[..], new_scratch, offset);
    assert!(scratchify_must_be_after + 6 < offset);

    ops.insert((scratchify_must_be_after*2 + offset)/3, Op::Scratchify{will_be_scratch: new_scratch});
    ops.insert((scratchify_must_be_after + offset*2)/3, Op::CommitNewScratch);
}

fn main() {
    let mut asm = Assembler::new();
    
    asm.global("foozle2");
    let reg = [
        [Ymm0, Ymm1, Ymm2, Ymm3],
        [Ymm4, Ymm5, Ymm6, Ymm7],
        [Ymm8, Ymm9, Ymm10, Ymm11],
        [Ymm12, Ymm13, Ymm14, Ymm15],
    ];
    
    for row in 0..4 {
        for col in 0..4 {
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
    
    let scratch0 = Ymm12;
    let scratch1 = Ymm10;
    
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
    
    let scratch_offsets = (128, 128+32);
    let mut scratch = scratch0;
    let mut scratch_offset = scratch_offsets.0;
    let mut pending_scratch: Option<(HWord, i32)> = None;
    
    // Set up our initial scratch register.
    asm.vmovupd(Rsp.value_at_offset(scratch_offset), scratch);
    
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
    
    // Restore the scratch into a register
    asm.vmovupd(scratch, Rsp.value_at_offset(scratch_offset));
    
    for (idx, reg) in ymms.iter().enumerate() {
        asm.vmovupd(Rdx.value_at_offset((idx as i32)*32), *reg);
    }
    
    asm.ret(no_arg());
    
    
    asm.output();
    
    
}
