use std::arch::x86_64::*;

use crate::advent::{day, Error};

type Input = (Vec<i32>, Vec<i32>);

fn parse(text: &str) -> Result<Input, Error> {
    let mut a = vec![];
    let mut b = vec![];

    for line in text.lines() {
        let mut iter = line.split("   ").map(|n| str::parse::<i32>(n.trim()));

        a.push(iter.next().ok_or("Missing a")??);
        b.push(iter.next().ok_or("Missing b")??);
    }

    Ok((a, b))
}

fn solve_a((ls, rs): &Input) -> i32 {
    let mut ls = ls.clone();
    let mut rs = rs.clone();
    ls.sort_unstable();
    rs.sort_unstable();

    assert_eq!(ls.len(), rs.len(), "Vec len mismatch!");
    let len = ls.len();
    let mut result = 0;

    let mut offset = 0;
    while offset + 4 <= len {
        unsafe {
            let chunk_l = _mm_load_si128(ls[offset..].as_ptr() as *const __m128i);
            let chunk_r = _mm_load_si128(rs[offset..].as_ptr() as *const __m128i);

            let chunk_sub = _mm_sub_epi32(chunk_l, chunk_r);
            let chunk_abs = _mm_abs_epi32(chunk_sub);

            let temp = _mm_hadd_epi32(chunk_abs, chunk_abs);
            let temp = _mm_hadd_epi32(temp, temp);
            let temp = _mm_cvtsi128_si32(temp);
            result += temp;
        }

        offset += 4;
    }

    result
}

fn solve_b(input: &Input) -> i32 {
    2
}

day!(1, parse, solve_a, solve_b);
