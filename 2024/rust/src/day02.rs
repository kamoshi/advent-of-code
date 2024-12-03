use std::arch::x86_64::*;

use crate::advent::{day, Error};

type Input = Vec<Box<[i32]>>;

fn parse(text: &str) -> Result<Input, Error> {
    let mut data = vec![];
    let mut temp = [0; 8];

    for line in text.lines() {
        let mut ptr = 0;

        for word in line.split_whitespace() {
            temp[ptr] = word.parse::<i32>()?;
            ptr += 1;
        }

        data.push(Box::from(&temp[..ptr]));
    }

    Ok(data)
}

#[inline(always)]
fn check_increasing(diff: __m128i, mask: __m128i) -> i32 {
    unsafe {
        // check if increasing in range 0 < x < 4
        let increasing = _mm_and_si128(
            _mm_cmpgt_epi32(diff, _mm_set1_epi32(0)),
            _mm_cmplt_epi32(diff, _mm_set1_epi32(4)),
        );

        _mm_testc_si128(increasing, mask)
    }
}

#[inline(always)]
fn check_decreasing(diff: __m128i, mask: __m128i) -> i32 {
    unsafe {
        // check if decreasing in range -4 < x < 0
        let decreasing = _mm_and_si128(
            _mm_cmpgt_epi32(diff, _mm_set1_epi32(-4)),
            _mm_cmplt_epi32(diff, _mm_set1_epi32(0)),
        );

        _mm_testc_si128(decreasing, mask)
    }
}

fn check(line: &[i32]) -> bool {
    let len = line.len();
    assert!(4 < len && len < 9);

    const LANES: usize = size_of::<__m128i>() / size_of::<i32>();

    unsafe {
        let mask = _mm_set1_epi32(-1);

        // check first numbers 0..3
        let curr = _mm_loadu_si128(line[0..].as_ptr() as *const _);
        let next = _mm_loadu_si128(line[1..].as_ptr() as *const _);
        let diff = _mm_sub_epi32(next, curr);

        let inc = check_increasing(diff, mask);
        let dec = check_decreasing(diff, mask);

        // neither increasing nor decreasing
        if inc | dec == 0 {
            return false;
        }

        // check last numbers N-3..N
        let curr = _mm_loadu_si128(line[len - 1 - LANES..].as_ptr() as *const _);
        let next = _mm_loadu_si128(line[len - LANES..].as_ptr() as *const _);
        let diff = _mm_sub_epi32(next, curr);

        if inc == 1 {
            check_increasing(diff, mask) == 1
        } else {
            check_decreasing(diff, mask) == 1
        }
    }
}

fn solve_a(input: &Input) -> usize {
    input.iter().filter(|line| check(line)).count()
}

fn solve_b(input: &Input) -> i32 {
    2
}

day!(2, parse, solve_a, solve_b);
