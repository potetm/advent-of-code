mod common;

fn sum_to_2020(ns: Vec<i64>) -> Vec<i64> {
    for i in ns.iter() {
        for j in ns.iter() {
            for k in ns.iter() {
                if i + j + k == 2020 {
                    return vec![*i, *j, *k];
                }
            }
        }
    }

    panic!("Numbers not found!");
}

fn main() {
    println!("{}", common::read_std_in()
             .map(common::parse_i64s)
             .map(sum_to_2020)
             .map(|nums| {
                 return nums[0] * nums[1] * nums[2];
             })
             .unwrap());
}
