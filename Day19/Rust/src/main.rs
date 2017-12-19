use std::io::{self, BufRead};

#[derive(Debug, Copy, Clone)]
enum Direction {
    North,
    East,
    South,
    West
}

impl Direction {
    fn left(self) -> Direction {
        match self {
            Direction::North => Direction::West,
            Direction::East => Direction::North,
            Direction::South => Direction::East,
            Direction::West => Direction::South,
        }
    }

    fn right(self) -> Direction {
        match self {
            Direction::North => Direction::East,
            Direction::East => Direction::South,
            Direction::South => Direction::West,
            Direction::West => Direction::North,
        }
    }
}

type Map = Vec<Vec<char>>;
type Coord = (usize, usize);

fn find_entry(map: &Map) -> Option<Coord> {
    map[0].iter().position(|&c| c == '|').map(|x| (x, 0))
}

fn tile_at(map: &Map, pos: Coord, dir: Direction) -> (Coord, char) {
    let atcoord = match dir {
        Direction::South => (pos.0 as isize, pos.1 as isize + 1),
        Direction::North => (pos.0 as isize, pos.1 as isize - 1),
        Direction::East => (pos.0 as isize + 1, pos.1 as isize),
        Direction::West => (pos.0 as isize - 1, pos.1 as isize)
    };

    if atcoord.1 < 0 || atcoord.1 as usize >= map.len()
        || atcoord.0 < 0 || atcoord.0 as usize >= map[atcoord.1 as usize].len() {

        (pos, ' ')
    } else {
        ((atcoord.0 as usize, atcoord.1 as usize), map[atcoord.1 as usize][atcoord.0 as usize])
    }
}

fn main() {
    let stdin = io::stdin();

    let map: Map = stdin
        .lock()
        .lines()
        .filter_map(|c| c.ok().map(|l| l.chars().collect()))
        .collect();

    let mut pos = find_entry(&map).unwrap();
    let mut direction = Direction::South;
    let mut steps = 0;

    print!("Part 1: ");

    loop {
        match map[pos.1][pos.0] {
            '+' | ' ' => {
                // Figure out whether to keep going or veer off
                if tile_at(&map, pos, direction).1 == ' ' {
                    // Nothing ahead, figure out route
                    if tile_at(&map, pos, direction.left()).1 != ' ' {
                        direction = direction.left();
                    } else if tile_at(&map, pos, direction.right()).1 != ' ' {
                        direction = direction.right();
                    } else {
                        // Nothing ahead, nothing left or right
                        break;
                    }
                }
            },

            // Keep going
            '|' | '-' => {},

            a @ 'A' ... 'Z' => print!("{}", a),

            _ => {}
        }

        let newpos = tile_at(&map, pos, direction).0;

        if newpos == pos {
            break;
        }

        steps += 1;
        pos = newpos;
    }

    println!("");
    println!("Part 2: {} steps", steps);
}
