use criterion::{Criterion, criterion_group, criterion_main};
use manito::lexer::lex;
use std::hint::black_box;

// Use all-packages.nix from nixpkgs repository (a very large Nix source file) to benchmark the
// lexer.
const ALL_PACKAGES_NIX: &str = include_str!("../docs/all-packages.nix");

fn lexer_benchmark(c: &mut Criterion) {
    c.bench_function("all-packages.nix", |b| {
        b.iter(|| lex(black_box(ALL_PACKAGES_NIX)))
    });
}

criterion_group!(benches, lexer_benchmark);
criterion_main!(benches);
