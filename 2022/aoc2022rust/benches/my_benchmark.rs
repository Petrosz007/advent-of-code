use aoc2022rust::days::{self, day3, day4, Day};
use aoc2022rust::days::{day1, day2};
use criterion::{criterion_group, criterion_main, Criterion};

pub fn day1_benchmark(c: &mut Criterion) {
    c.bench_function("day1", |b| {
        b.iter(|| Day::<1, day1::ParsedInput>::solve_days_input(&days::Days::new()))
    });
}

pub fn day2_benchmark(c: &mut Criterion) {
    c.bench_function("day2", |b| {
        b.iter(|| Day::<2, day2::ParsedInput>::solve_days_input(&days::Days::new()))
    });
}

pub fn day3_benchmark(c: &mut Criterion) {
    c.bench_function("day3", |b| {
        b.iter(|| Day::<3, day3::ParsedInput>::solve_days_input(&days::Days::new()))
    });
}

pub fn day4_benchmark(c: &mut Criterion) {
    c.bench_function("day4", |b| {
        b.iter(|| Day::<4, day4::ParsedInput>::solve_days_input(&days::Days::new()))
    });
}

criterion_group!(
    benches,
    // day1_benchmark,
    // day2_benchmark,
    // day3_benchmark,
    day4_benchmark,
);
criterion_main!(benches);
