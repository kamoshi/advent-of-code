use std::arch::x86_64::*;
use std::collections::HashMap;
use std::mem::MaybeUninit;

use crate::advent::{day, Error};

type Input = (Vec<i32>, Vec<i32>);

fn parse(text: &str) -> Result<Input, Error> {
    let mut a = vec![];
    let mut b = vec![];

    for line in text.lines() {
        let (l, r) = line.split_once("   ").ok_or("Missing separator")?;

        a.push(l.parse()?);
        b.push(r.parse()?);
    }

    Ok((a, b))
}

fn solve_a((ls, rs): &Input) -> i32 {
    assert_eq!(ls.len(), rs.len(), "Vec len mismatch!");
    let mut ls = ls.clone();
    let mut rs = rs.clone();
    ls.sort_unstable();
    rs.sort_unstable();

    const LANES: usize = size_of::<__m256i>() / size_of::<i32>();

    let padded = ((ls.len() + LANES - 1) / LANES) * LANES;
    ls.resize(padded, 0);
    rs.resize(padded, 0);

    let mut result = 0;
    let mut offset = 0;
    while offset < padded {
        unsafe {
            let chunk_l = _mm256_loadu_si256(ls[offset..].as_ptr() as *const _);
            let chunk_r = _mm256_loadu_si256(rs[offset..].as_ptr() as *const _);

            let chunk_sub = _mm256_sub_epi32(chunk_l, chunk_r);
            let chunk_abs = _mm256_abs_epi32(chunk_sub);

            let mut temp = MaybeUninit::<__m256i>::uninit();
            _mm256_store_si256(temp.as_mut_ptr(), chunk_abs);
            let temp: [i32; 8] = std::mem::transmute(temp.assume_init());

            result += temp.into_iter().sum::<i32>();
        }

        offset += LANES;
    }

    result
}

fn solve_b((ls, rs): &Input) -> i32 {
    let mut counts = HashMap::<i32, i32>::new();

    for &r in rs {
        *counts.entry(r).or_insert(0) += 1;
    }

    let mut result = 0;
    for &l in ls {
        result += l * counts.get(&l).unwrap_or(&0);
    }

    result
}

day!(1, parse, solve_a, solve_b);
