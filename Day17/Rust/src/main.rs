const PUZZLE_INPUT: usize = 386;
const ROUNDS1: usize = 2017;
const ROUNDS2: usize = 50_000_000;

fn perform_round(vec: &mut Vec<i32>, steps: usize, inspos: usize, n: i32) -> usize {
    let new_inspos = ((inspos + steps) % vec.len()) + 1;

    vec.insert(new_inspos, n);

    new_inspos
}

fn main() {
    let mut init = vec![0];
    let mut cur = 0;

    init.reserve(50_000_000);

    for i in 0..ROUNDS1 {
        //println!("{:?}", init);

        cur = perform_round(&mut init, PUZZLE_INPUT, cur, i as i32 + 1);
    }

    println!("Part 1: {}", init[cur + 1]);

    cur = 0;
    let mut last1 = 0;

    /*
     * 0 always stays the same, simulate array mutations and only watch for cur == 1 to change
     */
    for i in 0..ROUNDS2 {
        cur = (cur + PUZZLE_INPUT) % (i + 1) + 1;

        if cur == 1 {
            last1 = i + 1;
        }
    }

    println!("Part 2: {}", last1);
}
