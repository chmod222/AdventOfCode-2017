use std::io::{self, BufRead};
use std::collections::HashMap;
use std::ops::AddAssign;

#[derive(Debug, Hash, PartialEq, Eq, Copy, Clone)]
struct Position(i32, i32);

#[derive(Debug, Hash, PartialEq, Eq, Copy, Clone)]
enum Tile {
    Clean,
    Infected,
    Weakened,
    Flagged
}

impl AddAssign for Position {
    fn add_assign(&mut self, other: Position) {
        self.0 += other.0;
        self.1 += other.1;
    }
}

// Dejavu
#[derive(Debug, Hash, PartialEq, Eq, Copy, Clone)]
enum Direction {
    North,
    East,
    South,
    West
}

impl Direction {
    fn left(&self) -> Direction {
        match *self {
            Direction::North => Direction::West,
            Direction::West => Direction::South,
            Direction::South => Direction::East,
            Direction::East => Direction::North
        }
    }

    fn right(&self) -> Direction {
        self.left().left().left()
    }

    fn as_vector(&self) -> Position {
        match *self {
            Direction::North => Position(0, -1),
            Direction::East => Position(1, 0),
            Direction::South => Position(0, 1),
            Direction::West => Position(-1, 0)
        }
    }
}

struct Virus {
    pos: Position,
    heading: Direction,

    num_infected: i32,
    num_cleaned: i32
}

impl Virus {
    fn update(&mut self, grid: &mut Grid, part2: bool) {
        let current_tile = grid.entry(self.pos).or_insert(Tile::Clean);

        // Part 2
        match *current_tile {
            Tile::Clean => {
                self.heading = self.heading.left();

                *current_tile = if part2 {
                    Tile::Weakened
                } else {
                    Tile::Infected
                };
            },

            Tile::Infected => {
                self.heading = self.heading.right();

                *current_tile = if part2 {
                    Tile::Flagged
                } else {
                    Tile::Clean
                };
            },

            Tile::Weakened => {
                *current_tile = Tile::Infected;
            },

            Tile::Flagged => {
                self.heading = self.heading.right().right();

                *current_tile = Tile::Clean;
            }
        }

        if *current_tile == Tile::Clean {
            self.num_cleaned += 1;
        } else if *current_tile == Tile::Infected {
            self.num_infected += 1;
        }

        // Go on
        self.pos += self.heading.as_vector();
    }
}

type Grid = HashMap<Position, Tile>;

fn main() {
    let stdin = io::stdin();

    let local_area = stdin
        .lock()
        .lines()
        .filter_map(io::Result::ok)
        .map(|l| l.chars().map(|c| if c == '#' {
            Tile::Infected
        } else {
            Tile::Clean
        }).collect::<Vec<_>>())
        .collect::<Vec<_>>();

    let mut grid_part1 = Grid::new();

    // assume the input gods give us square maps
    for y in 0..local_area.len() {
        for x in 0..local_area.len() {
            grid_part1.insert(Position(x as i32, y as i32), local_area[y][x]);
        }
    }

    let mut grid_part2 = grid_part1.clone();

    let half = local_area.len() as i32 / 2;
    let center = Position(half, half);

    let mut virus = Virus {
        pos: center,
        heading: Direction::North,

        num_infected: 0,
        num_cleaned: 0
    };

    for _ in 0..10_000 {
        virus.update(&mut grid_part1, false);
    }

    println!("Part 1: {} infected, {} cleaned",
         virus.num_infected,
         virus.num_cleaned);

    virus = Virus {
        pos: center,
        heading: Direction::North,

        num_infected: 0,
        num_cleaned: 0
    };

    for _ in 0..10_000_000 {
        virus.update(&mut grid_part2, true);
    }

    println!("Part 2: {} infected, {} cleaned",
         virus.num_infected,
         virus.num_cleaned);
}
