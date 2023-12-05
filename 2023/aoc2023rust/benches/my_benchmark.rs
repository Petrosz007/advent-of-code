use aoc2023rust::days::{self, day3, Day};
use criterion::{criterion_group, criterion_main, Criterion};

pub fn day3_benchmark(c: &mut Criterion) {
    c.bench_function("day3", |b| {
        b.iter(|| Day::<3, day3::ParsedInput>::solve_days_input(&days::Days::new()))
    });
}

criterion_group!(
    benches,
    // day1_benchmark,
    // day2_benchmark,
    day3_benchmark,
    // day4_benchmark,
    // day5_benchmark,
);
criterion_main!(benches);
