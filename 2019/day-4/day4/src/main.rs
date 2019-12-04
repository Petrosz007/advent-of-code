extern crate digits_iterator;
use digits_iterator::*;

pub fn has_same(x: &u32) -> bool {
    let mut iter = x.digits();
    let mut prev = iter.next().unwrap();
    for d in iter {
        if d == prev {
            return true;
        }
        prev = d;
    }
    false
}

pub fn has_same2(x: &u32) -> bool {
    let mut iter = x.digits();
    let mut prevvec: Vec<(u8, i32)> = Vec::new();
    prevvec.push((iter.next().unwrap(), 1));
    for d in iter {
        //println!("1: {} {}", prev, d);
        let (c, count) = prevvec.last().unwrap();
        if d == *c {
            let len = prevvec.len() - 1;
            prevvec[len] = (*c, *count + 1);    
        } else {
            prevvec.push((d, 1));
        }
    }
    
    prevvec.iter().filter(|(_, x)| *x == 2).count() != 0
}

pub fn is_increasing(x: &u32) -> bool {
    let mut iter = x.digits();
    let mut prev = iter.next().unwrap();
    for d in iter {
        if d < prev {
            return false;
        }
        prev = d;
    }

    true
}

fn main() {
    let min = 245182;
    let max = 790572;

    let count1 = (min..=max).filter(|x| is_increasing(x) && has_same(x)).count();
    let count2 = (min..=max).filter(|x| is_increasing(x) && has_same2(x)).count();
    println!("Part 1: {}", count1);
    println!("Part 2: {}", count2);
}