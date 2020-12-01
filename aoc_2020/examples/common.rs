use std::io::{self, Read};

pub fn parse_i64s(s: String) -> Vec<i64> {
    s.lines()
        .map(|l| match l.parse::<i64>() {
            Ok(parsed) => parsed,
            Err(e) => panic!("Error parsing string to int: {}", e)
        })
        .collect()
}

pub fn read_std_in() -> io::Result<String> {
    let mut buffer = String::new();
    let mut stdin  = io::stdin();
    stdin.read_to_string(&mut buffer)?;
    Ok(buffer)
}
