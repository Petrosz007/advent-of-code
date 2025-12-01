use std::hash::Hash;
use std::str::FromStr;
use std::{borrow::Cow, collections::HashSet};

use once_cell::sync::Lazy;
use regex::{Regex, Replacer, Split};

pub trait ReducibleIterator: Iterator {
    fn reduce<F>(mut self, mut f: F) -> Option<Self::Item>
    where
        Self: Sized,
        F: FnMut(Self::Item, Self::Item) -> Self::Item,
    {
        let mut acc = self.next()?;
        for x in self {
            acc = f(acc, x);
        }
        Some(acc)
    }
}

pub fn intersect<T>(inputs: &[Vec<T>]) -> Vec<T>
where
    T: Hash + Eq + Clone,
{
    inputs
        .iter()
        .map(|coll| coll.iter().cloned().collect::<HashSet<T>>())
        .reduce(|acc, set| acc.intersection(&set).cloned().collect::<HashSet<T>>())
        .map_or_else(|| Vec::new(), |coll| coll.iter().cloned().collect())
}

pub fn transpose<T>(v: &Vec<Vec<T>>) -> Vec<Vec<T>>
where
    T: Clone,
{
    assert!(!v.is_empty());
    (0..v[0].len())
        .map(|i| v.iter().map(|inner| inner[i].clone()).collect::<Vec<T>>())
        .collect()
}

pub fn transpose_lines(lines: &[&str]) -> Vec<String> {
    let chars_2d_matrix: Vec<Vec<char>> = lines.iter().map(|line| line.chars().collect()).collect();

    let transposed = transpose(&chars_2d_matrix);

    transposed.iter().map(|xs| xs.iter().collect()).collect()
}

pub trait StrExtensions {
    fn regex_replace_all<'t, R: Replacer>(&'t self, re: &Regex, rep: R) -> Cow<'t, str>;
    fn regex_split<'r, 't>(&'t self, re: &'r Regex) -> Split<'r, 't>;

    fn whitespace_split_to<G: FromStr>(&self) -> Box<dyn Iterator<Item = G> + '_>
    where
        <G as std::str::FromStr>::Err: std::fmt::Debug;
}

static WHITESPACE_REGEX: Lazy<Regex> = Lazy::new(|| Regex::new(r#"\s+"#).unwrap());

impl StrExtensions for str {
    fn regex_replace_all<'t, R: Replacer>(&'t self, re: &Regex, rep: R) -> Cow<'t, str> {
        re.replace_all(self, rep)
    }

    fn regex_split<'r, 't>(&'t self, re: &'r Regex) -> Split<'r, 't> {
        re.split(self)
    }

    fn whitespace_split_to<G: FromStr>(&self) -> Box<dyn Iterator<Item = G> + '_>
    where
        <G as std::str::FromStr>::Err: std::fmt::Debug,
    {
        Box::new(
            self.trim()
                .regex_split(&WHITESPACE_REGEX)
                .map(|s| s.parse::<G>().unwrap()),
        )
    }
}
