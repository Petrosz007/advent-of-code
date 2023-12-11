use aoc2023rust::days::{day1, day10, day2, day3, day4, day6, Day};
use criterion::{criterion_group, criterion_main, Criterion};

pub fn day1_benchmark(c: &mut Criterion) {
    c.bench_function("day1", |b| b.iter(day1::Day1::solve_days_input));
}

pub fn day2_benchmark(c: &mut Criterion) {
    c.bench_function("day2", |b| b.iter(day2::Day2::solve_days_input));
}
pub fn day3_benchmark(c: &mut Criterion) {
    c.bench_function("day3", |b| b.iter(day3::Day3::solve_days_input));
}
pub fn day4_benchmark(c: &mut Criterion) {
    c.bench_function("day4", |b| b.iter(day4::Day4::solve_days_input));
}
pub fn day6_benchmark(c: &mut Criterion) {
    c.bench_function("day6", |b| b.iter(day6::Day6::solve_days_input));
}
pub fn day10_benchmark(c: &mut Criterion) {
    c.bench_function("day10", |b| b.iter(day10::Day10::solve_days_input));
}

criterion_group!(
    benches,
    // day1_benchmark,
    // day2_benchmark,
    // day3_benchmark,
    // day4_benchmark,
    // day6_benchmark,
    day10_benchmark,
);
criterion_main!(benches);
