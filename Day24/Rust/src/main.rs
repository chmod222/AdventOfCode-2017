use std::io::{self, BufRead};
use std::cmp::max;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
struct Component(i32, i32);

impl Component {
    fn from_string(raw: &str) -> Option<Component> {
        let mut it = raw.split("/");

        Some(Component(it.next()?.parse().ok()?, it.next()?.parse().ok()?))
    }

    fn matches(&self, port: i32) -> bool {
        self.0 == port || self.1 == port
    }
}

fn build_bridge(comps: &[Component], req: i32, init: &[Component], mut all_found: &mut Vec<Vec<Component>>) {
    let follow = comps.iter().filter(|c1| c1.matches(req) && init.iter().find(|c2| c1 == c2).is_none());

    for f in follow {
        let nreq = if f.0 == req { f.1 } else { f.0 };

        let mut ninit = init.to_vec();
        ninit.push(*f);

        build_bridge(&comps, nreq, &ninit, &mut all_found);

        all_found.push(ninit);
    }
}

fn bridge_strength(bridge: &[Component]) -> i32 {
    bridge.iter().fold(0, |acc, seg| acc + (seg.0 + seg.1))
}

fn main() {
    let stdin = io::stdin();
    let input = stdin
        .lock()
        .lines()
        .filter_map(|l| Component::from_string(&l.ok()?))
        .collect::<Vec<_>>();

    let mut all = Vec::new();

    for component in &input {
        if component.matches(0) {
            build_bridge(&input, max(component.0, component.1), &[*component], &mut all);
        }
    }

    // Let's solve an O(n) problem in O(n * log n) because lazy
    all.sort_by(|b1, b2| bridge_strength(b2).cmp(&bridge_strength(b1)));
    println!("Part 1: {}", bridge_strength(&all[0]));

    all.sort_by(|b1, b2| b2.len().cmp(&b1.len()));
    println!("Part 2: {}", bridge_strength(&all[0]));
}
