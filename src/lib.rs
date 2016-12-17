extern "C" {
    fn chacha_avx(input_block: *const u32, output_blocks: *mut u8, doublerounds: u8) -> u32;
}

pub fn chacha20_permute_words(input_block: &[u32; 16], output_blocks: &mut [u32; 64]) {
    unsafe {
        chacha_avx(input_block.as_ptr(), output_blocks.as_mut_ptr() as *mut u8, 10);
    }
}

pub fn encrypt(input_block: &[u32; 16], output_blocks: &mut [u8; 256], doublerounds: u8) {
    unsafe {
        chacha_avx(input_block.as_ptr(), output_blocks.as_mut_ptr(), doublerounds);
    }
}

#[test]
fn twentyround() {
    let xs: [u32; 16] = [
        11001100, 22002200, 33003300, 44004400, 55005500, 66006600, 77007700, 88008800,
        10101010, 20202020, 30303030, 40404040, 50505050, 60606060, 70707070, 80808080,
    ];
    let mut result = [0u32; 16*4];
    result[0] = 1363452;
    let initial_result = result.to_vec();
    chacha20_permute_words(&xs, &mut result);
    
    let permuted: Vec<u32> = [
        101210900,
        2837022664,
        272923777,
        2835915203,
        1550936622,
        3340571258,
        441187412,
        3793039629,
        1848512408,
        3025034551,
        575569825,
        1552622693,
        3537415065,
        3304841794,
        4055228093,
        4221643734,
        2874442094,
        1321848398,
        920977640,
        4196189244,
        4198986539,
        1049130787,
        1291375432,
        3554522033,
        1532506604,
        1387245899,
        3299380405,
        3921501686,
        2119325061,
        3096349518,
        1909573126,
        3877779024,
        2872918592,
        1667345534,
        851606475,
        1205391499,
        3874334756,
        2618053686,
        2039688798,
        2048250428,
        348879397,
        489657010,
        4155375044,
        4099989289,
        1506257749,
        63391246,
        64647877,
        1036059415,
        4038537053,
        504474319,
        577837403,
        795802521,
        3371271745,
        346815338,
        1286859034,
        3860985279,
        2104525039,
        2170620849,
        1199432617,
        2892606714,
        2890970992,
        3911495814,
        3501289299,
        1885844014,
    ].to_vec();
    
    let mut initial_with_counters = Vec::new();
    initial_with_counters.extend(&xs[..]);
    initial_with_counters.extend(&xs[..]);
    initial_with_counters[32-1] += 1;
    initial_with_counters.extend(&xs[..]);
    initial_with_counters[48-1] += 2;
    initial_with_counters.extend(&xs[..]);
    initial_with_counters[64-1] += 3;
    
    let added: Vec<u32> = permuted.iter().zip(initial_with_counters.iter()).map(|(x, y)| (*x).wrapping_add(*y)).collect();
    
    let _permuted_and_xored: Vec<u32> = permuted.iter().zip(initial_result.iter()).map(|(x, y)| *x ^ *y).collect();
    let encrypted: Vec<u32> = added.iter().zip(initial_result.iter()).map(|(x, y)| *x ^ *y).collect();
    
    for (p, a) in permuted.iter().zip(result.iter()) {
        println!("{} {}", *p, *a);
    }
    
    assert_eq!(result.to_vec(), permuted);
    
}
