#include <iostream>
#include <algorithm>
#include <vector>
#include <set>
#include <memory>
#include <string>
#include <unordered_map>
#include <sstream>

struct node {
	node(std::string name, int weight)
		: name{name},
		  weight{weight}
	{
	}

	std::string name;
	int weight;

	std::shared_ptr<node> parent;
	std::vector<std::shared_ptr<node>> children;
};

std::vector<std::string> split(std::string const& s, char delimiter)
{
   std::vector<std::string> tokens;
   std::string token;
   std::istringstream tokenStream(s);

   while (std::getline(tokenStream, token, delimiter)) {
      tokens.push_back(token);
   }

   return tokens;
}

void read_nodes(
	std::istream& strm,
	std::unordered_map<std::string, std::shared_ptr<node>>& nodes)
{
	std::string line;
	std::unordered_map<std::string, std::vector<std::string>> assocs;

	// Read all nodes
	do {
		std::getline(strm, line);

		if (!strm.eof()) {
			//std::cout << line << std::endl;

			std::stringstream str{line};

			std::string name;
			std::string weight;
			std::string trail;

			str >> name;
			str >> weight;
			str >> trail;

			nodes[name] = std::make_shared<node>(name, std::atoi(weight.substr(1, weight.size() - 2).c_str()));

			std::vector<std::string> children;

			if (!trail.empty()) {
				do {
					str >> trail;

					if (trail[trail.size() - 1] == ',') {
						trail = trail.substr(0, trail.size() - 1);
					}

					children.push_back(trail);
				} while (!trail.empty() && !str.eof());
			}

			assocs[name] = std::move(children);
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

int total_weight(node const& base)
{
	int w = base.weight;

	for (auto const& c: base.children) {
		w += total_weight(*c);
	}

	return w;
}

std::shared_ptr<node> find_outlier(std::shared_ptr<node> root, int& diff)
{
	std::multiset<int> weights{};

	for (auto const& n : root->children) {
		weights.insert(total_weight(*n));
	}

	int min = *std::min_element(std::begin(weights), std::end(weights));
	int max = *std::max_element(std::begin(weights), std::end(weights));

	diff = max - min;

	if (diff != 0) {
		int i = 0;

		for (auto const& w : weights) {
			if (weights.count(w) == (weights.size() - 1)) {
				return root->children[i];
			}

			i++;
		}
	}

	return std::shared_ptr<node>{nullptr};
}

std::shared_ptr<node> find_imbalance(std::shared_ptr<node> base, int& diff)
{
	int ldiff;
	std::shared_ptr<node> outlier = find_outlier(base, ldiff);

	if (ldiff) {
		diff = ldiff;
	}

	if (!outlier) {
		return base;
	}

	return find_imbalance(outlier, diff);
}

int main(int argc, char* argv[])
{
	std::unordered_map<std::string, std::shared_ptr<node>> nodes;

	read_nodes(std::cin, nodes);

	std::shared_ptr<node> root{nullptr};

	// Find parent (part 1)
	for (auto const& entry : nodes) {
		//std::cout << entry.second->name << ": " << entry.second->children.size() << std::endl;

		if (!entry.second->parent) {
			root = entry.second;
			std::cout << "Part 1: root = " << entry.second->name << std::endl;
		}
	}

	// Find off-balance node (part 2)
	int base_weight = total_weight(*root);
	int diff;

	std::shared_ptr<node> outlier = find_imbalance(root, diff);
	std::cout << "Part 2: outlier = " << outlier->name << std::endl;
	std::cout << "Outlier weight is: " << outlier->weight << "; should be " << outlier->weight - diff << std::endl;

	return 0;
}
