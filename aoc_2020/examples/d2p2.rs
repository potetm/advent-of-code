mod common;

use regex::Regex;

#[derive(PartialEq, Eq, Debug, Clone)]
struct Input {
    pos1: i64,
    pos2: i64,
    c: char,
    pass: String,
}

fn parse(s: String) -> Vec<Input> {
    let re = Regex::new(r"(?m)^(?P<min>\d+)-(?P<max>\d+) (?P<c>[a-z]): (?P<pass>[a-z]+)$").unwrap();
    return re.captures_iter(&s)
        .map(|c| {
            Input {
                pos1: c.name("min")
                    .iter()
                    .flat_map(|m| {
                        m.as_str().parse::<i64>()
                    })
                    .collect::<Vec<i64>>()[0],
                pos2: c.name("max")
                    .iter()
                    .flat_map(|m| {
                        m.as_str().parse::<i64>()
                    })
                    .collect::<Vec<i64>>()[0],
                c:   c.name("c")
                    .iter()
                    .flat_map(|m| {
                        m.as_str()
                            .chars()
                            .next()
                    })
                    .collect::<Vec<char>>()[0],
                pass: c.name("pass")
                    .map(|m| {
                        m.as_str().to_string()
                    })
                    .unwrap(),
            }
        })
        .collect();
}

fn is_valid(i: &Input) -> bool {
    let c1 = i.pass.chars().nth(i.pos1 as usize - 1).unwrap();
    let c2 = i.pass.chars().nth(i.pos2 as usize - 1).unwrap();

    (c1 == i.c || c2 == i.c) && c1 != c2
}

fn main() {
    println!("{:?}", common::read_std_in()
             .map(|l| parse(l).into_iter().filter(is_valid).count())
             .unwrap());
}
