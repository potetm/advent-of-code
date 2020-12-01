use std::collections::HashSet;

mod common;

fn sum_to_2020(ns: Vec<i64>) -> Vec<i64> {
    let mut seen = HashSet::<i64>::new();

    for n in ns.iter() {
        let i = 2020 - *n;

        if seen.contains(&i) {
            return vec![*n, i];
        }

        seen.insert(*n);
    }

    panic!("Numbers not found!");
}

fn main() {
    println!("{}", common::read_std_in()
             .map(common::parse_i64s)
             .map(sum_to_2020)
             .map(|nums| {
                 return nums[0] * nums[1];
             })
             .unwrap());
}
