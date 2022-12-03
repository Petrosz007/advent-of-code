use std::collections::HashSet;
use std::hash::Hash;

pub trait ReducibleIterator: Iterator {
    fn reduce<F>(mut self, mut f: F) -> Option<Self::Item>
    where
        Self: Sized,
        F: FnMut(Self::Item, Self::Item) -> Self::Item,
    {
        let mut acc = self.next()?;
        while let Some(x) = self.next() {
            acc = f(acc, x);
        }
        Some(acc)
    }
}

pub fn intersect<T>(inputs: &Vec<Vec<T>>) -> Vec<T>
where
    T: Hash + Eq + Clone,
{
    let result = inputs
        .iter()
        .map(|coll| HashSet::<T>::from_iter(coll.iter().cloned()))
        .reduce(|acc, set| acc.intersection(&set).cloned().collect::<HashSet<T>>());

    match result {
        Some(coll) => coll.iter().cloned().collect(),
        None => Vec::new(),
    }
}
