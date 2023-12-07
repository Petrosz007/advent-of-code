use aoc2023rust::days::{self, day3, day6, Day};
use criterion::{criterion_group, criterion_main, Criterion};

pub fn day3_benchmark(c: &mut Criterion) {
    c.bench_function("day3", |b| {
        b.iter(|| Day::<3, day3::ParsedInput>::solve_days_input(&days::Days::new()))
    });
}
pub fn day6_benchmark(c: &mut Criterion) {
    c.bench_function("day6", |b| {
        b.iter(|| Day::<6, day6::ParsedInput>::solve_days_input(&days::Days::new()))
    });
}

criterion_group!(
    benches,
    // day3_benchmark,
    day6_benchmark,
);
criterion_main!(benches);
