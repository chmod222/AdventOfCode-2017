#include <iostream>
#include <algorithm>
#include <vector>
#include <set>
#include <memory>
#include <string>
#include <unordered_map>
#include <sstream>

struct node {
	node(int id)
		: id{id},
		  counted{false},
		  group{-1}
	{
	}

	int group;
	int id;
	mutable bool counted;

	std::shared_ptr<node> parent;
	std::vector<std::shared_ptr<node>> children;
};

void read_nodes(
	std::istream& strm,
	std::unordered_map<int, std::shared_ptr<node>>& nodes)
{
	std::string line;
	std::unordered_map<int, std::vector<int>> assocs;

	// Read all nodes
	do {
		std::getline(strm, line);

		if (!strm.eof()) {
			//std::cout << line << std::endl;

			std::stringstream str{line};

			int id;
			std::string trail;

			str >> id;
			str >> trail;

			nodes[id] = std::make_shared<node>(id);

			std::vector<int> children;

			if (!trail.empty()) {
				do {
					str >> trail;

					if (trail[trail.size() - 1] == ',') {
						trail = trail.substr(0, trail.size() - 1);
					}

					children.push_back(std::atoi(trail.c_str()));
				} while (!trail.empty() && !str.eof());
			}

			assocs[id] = std::move(children);
		}
	} while (!strm.eof());

	// Associate children
	for (auto const& kvp : assocs) {
		std::shared_ptr<node> n = nodes[kvp.first];

		for (auto const& child : kvp.second) {
			n->children.push_back(nodes[child]);
			nodes[child]->parent = n;
		}
	}
}

int count_children(node const& base)
{
	int total = 1; // self

	base.counted = true;

	for (auto const& child : base.children) {
		if (!child->counted) {
			total += count_children(*child);
		}
	}

	return total;
}

int discover_group(node const& base)
{
	if (base.group >= 0) {
		return base.group;
	} else {
		for (auto const& child : base.children) {
			if (child->counted) {
				continue;
			}

			child->counted = true;

			int child_group = discover_group(*child);

			if (child_group >= 0) {
				return child_group;
			}
		}
	}

	return -1;
}

int count_groups(std::unordered_map<int, std::shared_ptr<node>> const& nodes)
{
	int group = 0;

	for (auto& node : nodes) {
		for (auto& node : nodes) {
			node.second->counted = false;
		}

		if (node.second->group >= 0) {
			continue;
		}

		int group_discover = discover_group(*node.second);

		if (group_discover < 0) {
			node.second->group = group++;
		}
	}

	return group;
}

int main(int argc, char* argv[])
{
	std::unordered_map<int, std::shared_ptr<node>> nodes;

	read_nodes(std::cin, nodes);

	auto part1 = count_children(*nodes[0]);
	std::cout << "Part 1: " << part1 << std::endl;

	auto part2 = count_groups(nodes);
	std::cout << "Part 2: " << part2 << std::endl;

	return 0;
}
