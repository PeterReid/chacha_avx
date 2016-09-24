extern "C" {
    fn chacha_avx(xs: *const u32, result: *mut u32) -> u32;
}

fn main() {
    let xs: [u32; 16] = [
        11001100, 22002200, 33003300, 44004400, 55005500, 66006600, 77007700, 88008800,
        10101010, 20202020, 30303030, 40404040, 50505050, 60606060, 70707070, 80808080,
    ];
    let mut result = [0u32; 16*4];
    println!("{:p}", &xs[..]);
    println!("Hello, world! {}", unsafe { chacha_avx(xs.as_ptr(), result.as_mut_ptr()) });
    
    println!("Final = {:?}", result.to_vec());
}
