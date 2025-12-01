use aoc2025rust::days::{Day, day1};
use criterion::{Criterion, criterion_group, criterion_main};

pub fn day1_benchmark(c: &mut Criterion) {
    c.bench_function("day1", |b| b.iter(day1::Day1::solve_days_input));
}

criterion_group!(
    benches,
    day1_benchmark,
    // day2_benchmark,
    // day3_benchmark,
    // day4_benchmark,
    // day6_benchmark,
    // day10_benchmark,
);
criterion_main!(benches);
