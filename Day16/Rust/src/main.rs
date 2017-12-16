#[derive(Debug)]
enum Pattern {
    Spin(usize),
    Exchange(usize, usize),
    Partner(char, char),
}

impl Pattern {
    fn parse(inp: &str) -> Option<Pattern> {
        let (lead, trail) = inp.split_at(1);

        match lead {
            "s" => Some(Pattern::Spin(trail.parse().ok()?)),
            "x" => {
                let mut parts = trail.split('/');
                let a = parts.next()?.parse().ok()?;
                let b = parts.next()?.parse().ok()?;

                Some(Pattern::Exchange(a, b))
            }

            "p" => {
                let mut parts = trail.split('/');
                let a = parts.next()?.parse().ok()?;
                let b = parts.next()?.parse().ok()?;

                Some(Pattern::Partner(a, b))
            }

            _ => None,
        }
    }
}

fn rotate<T: Copy>(arr: &mut [T]) {
    let tmp = arr[0];

    let mut i: usize = 0;

    while i < arr.len() - 1 {
        arr[i] = arr[i + 1];

        i += 1;
    }

    arr[i] = tmp;
}

fn rotate_n<T: Copy>(n: usize, arr: &mut [T]) {
    for _ in 0..(arr.len() - n) {
        rotate(arr);
    }
}

fn dance(group: &mut [char], pattern: &Pattern) -> () {
    match *pattern {
        Pattern::Spin(n) => rotate_n(n, group),

        Pattern::Exchange(a, b) => {
            let tmp = group[a];

            group[a] = group[b];
            group[b] = tmp;
        }

        Pattern::Partner(a, b) => {
            let apos = group.iter().position(|&c| c == a).unwrap();
            let bpos = group.iter().position(|&c| c == b).unwrap();

            dance(group, &Pattern::Exchange(apos, bpos));
        }
    }
}

fn main() {
    let dancers_init = ['a', 'b', 'c', 'd',
                        'e', 'f', 'g', 'h',
                        'i', 'j', 'k', 'l',
                        'm', 'n', 'o', 'p'];

    let mut line = String::new();

    std::io::stdin().read_line(&mut line).unwrap();

    let mut dancers = dancers_init;
    let input = line.trim()
        .split(',')
        .filter_map(Pattern::parse)
        .collect::<Vec<_>>();

    for p in &input {
        dance(&mut dancers, p);
    }

    println!("Part 1: {}", dancers.iter().collect::<String>());

    // Reset for part 2
    dancers = dancers_init;

    let mut i = 0;

    while i < 1_000_000_000 {
        for p in &input {
            dance(&mut dancers, p);
        }

        i += 1;

        if dancers == dancers_init {
            // Found cycle at i+1 -> skip to the end
            i = (1_000_000_000 / i) * i;
        }
    }

    println!("Part 2: {}", dancers.iter().collect::<String>());
}
