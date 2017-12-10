fn part1(input: &[u32]) -> u32 {
    let mut total = 0;

    for i in 0 .. input.len() {
        if input[(i + 1) % input.len()] == input[i] {
            total += input[i];
        }
    }

    total
}

fn part2(input: &[u32]) -> u32 {
    let mut total = 0;

    for i in 0 .. input.len() {
        if input[(i + input.len() / 2) % input.len()] == input[i] {
            total += input[i];
        }
    }

    total
}

fn main() {
    let mut line = String::new();

    std::io::stdin().read_line(&mut line).unwrap();

    let input = line
        .chars()
        .filter_map(|c| c.to_digit(10))
        .collect::<Vec<_>>();

    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input));
}
