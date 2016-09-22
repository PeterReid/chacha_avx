extern "C" {
    fn foozle2(xs: *const u32, result: *mut u32) -> u32;
}

fn main() {
    let xs: [u32; 8] = [11001100, 22002200, 33003300, 44004400, 55005500, 66006600, 77007700, 88008800];
    let mut result = [0u32; 8*8];
    println!("{:p}", &xs[..]);
    println!("Hello, world! {}", unsafe { foozle2(xs.as_ptr(), result.as_mut_ptr()) });
}
