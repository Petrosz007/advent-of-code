use std::collections::HashSet;
use std::hash::Hash;

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
