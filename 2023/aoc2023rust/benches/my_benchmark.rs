use aoc2023rust::days::{day1, day3, day6, Day};
use criterion::{criterion_group, criterion_main, Criterion};

pub fn day1_benchmark(c: &mut Criterion) {
    c.bench_function("day1", |b| b.iter(day1::Day1::solve_days_input));
}
pub fn day3_benchmark(c: &mut Criterion) {
    c.bench_function("day3", |b| b.iter(day3::Day3::solve_days_input));
}
pub fn day6_benchmark(c: &mut Criterion) {
    c.bench_function("day6", |b| b.iter(day6::Day6::solve_days_input));
}

criterion_group!(
    benches,
    day1_benchmark,
    // day3_benchmark,
    // day6_benchmark,
);
criterion_main!(benches);
