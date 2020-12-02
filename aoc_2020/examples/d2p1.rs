mod common;

use regex::Regex;

#[derive(PartialEq, Eq, Debug, Clone)]
struct Input {
    min: i64,
    max: i64,
    c: char,
    pass: String,
}

fn parse(s: String) -> Vec<Input> {
    let re = Regex::new(r"(?m)^(?P<min>\d+)-(?P<max>\d+) (?P<c>[a-z]): (?P<pass>[a-z]+)$").unwrap();
    return re.captures_iter(&s)
        .map(|c| {
            Input {
                min: c.name("min")
                    .iter()
                    .flat_map(|m| {
                        m.as_str().parse::<i64>()
                    })
                    .collect::<Vec<i64>>()[0],
                max: c.name("max")
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
    let count = i.pass
              .chars()
              .filter(|c| c == &i.c)
              .count() as i64;

    i.min <= count && count <= i.max
}

fn main() {
    println!("{:?}", common::read_std_in()
             .map(|l| parse(l).into_iter().filter(is_valid).count())
             .unwrap());
}
