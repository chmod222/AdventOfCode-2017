use std::fmt;

use std::collections::HashMap;

struct Band {
    data: Vec<bool>,
    real_position: usize
}

const GROW_FACTOR: usize = 8;

impl Band {
    fn new() -> Band {
        Band {
            data: vec![false],
            real_position: 0
        }
    }

    fn write(&mut self, value: bool) {
        self.data[self.real_position] = value;
    }

    fn read(&mut self) -> bool {
        self.data[self.real_position]
    }

    fn move_left(&mut self) {
        if self.real_position == 0 {
            self.grow(GROW_FACTOR);
        }

        self.real_position -= 1;
    }

    fn checksum(&self) -> usize {
        self.data.iter().filter(|&b| *b).count()
    }

    fn move_right(&mut self) {
        if self.real_position == self.data.len() - 1 {
            self.grow(GROW_FACTOR);
        }

        self.real_position += 1;
    }

    fn grow(&mut self, elems: usize) {
        for _ in 0..elems/2 {
            self.data.insert(0, false);
        }

        for _ in 0..elems/2 {
            self.data.push(false);
        }

        self.real_position += elems /  2;
    }
}

impl fmt::Display for Band {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for i in 0..self.data.len() {
            if i == self.real_position as usize {
                write!(f, "[")?;
            } else {
                write!(f, " ")?;
            }

            write!(f, "{}", self.data[i] as i8)?;

            if i == self.real_position as usize {
                write!(f, "]")?;
            } else {
                write!(f, " ")?;
            }
        }

        Ok(())
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum Move { Left, Right }

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
enum State {
    // v-- TEST INPUT
    A, B, C, D, E, F
    // REAL INPUT -^
}

// (when 0, when 1)
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
struct StateActions(StateAction, StateAction);

// (value_to_set, direction_to_move, next_state)
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
struct StateAction(bool, Move, State);

struct Blueprint {
    start_state: State,
    checksum_after: usize,

    actions: HashMap<State, StateActions>
}

fn test_blueprint() -> Blueprint {
    let mut actions = HashMap::new();

    actions.insert(State::A, StateActions(
        StateAction(true, Move::Right, State::B),
        StateAction(false, Move::Left, State::B)));

    actions.insert(State::B, StateActions(
        StateAction(true, Move::Left, State::A),
        StateAction(true, Move::Right, State::A)));

    Blueprint {
        start_state: State::A,
        checksum_after: 6,

        actions: actions
    }
}

/*
 * Not going to write a parser for this one
 */
fn actual_blueprint() -> Blueprint {
    let mut actions = HashMap::new();

    actions.insert(State::A, StateActions(
        StateAction(true,  Move::Right, State::B),
        StateAction(false, Move::Left,  State::C)));

    actions.insert(State::B, StateActions(
        StateAction(true,  Move::Left,  State::A),
        StateAction(true,  Move::Left, State::D)));

    actions.insert(State::C, StateActions(
        StateAction(true,  Move::Right, State::D),
        StateAction(false, Move::Right, State::C)));

    actions.insert(State::D, StateActions(
        StateAction(false, Move::Left,  State::B),
        StateAction(false, Move::Right, State::E)));

    actions.insert(State::E, StateActions(
        StateAction(true,  Move::Right, State::C),
        StateAction(true,  Move::Left,  State::F)));

    actions.insert(State::F, StateActions(
        StateAction(true,  Move::Left,  State::E),
        StateAction(true,  Move::Right, State::A)));

    Blueprint {
        start_state: State::A,
        checksum_after: 12172063,

        actions: actions
    }
}

fn run_turing_machine(blueprint: &Blueprint) -> usize {
    let mut b = Band::new();
    let mut state = blueprint.start_state;

    for _ in 0..blueprint.checksum_after {
        let actions = if !b.read() {
            blueprint.actions[&state].0
        } else {
            blueprint.actions[&state].1
        };

        b.write(actions.0);
        
        match actions.1 {
            Move::Left => b.move_left(),
            Move::Right => b.move_right()
        }

        state = actions.2;
    }

    b.checksum()
}

fn main() {
    let chk1 = run_turing_machine(&actual_blueprint());

    println!("Part 1: {}", chk1);
}
