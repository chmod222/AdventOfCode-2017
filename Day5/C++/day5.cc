#include <iostream>
#include <vector>

std::vector<int> read_input(std::istream& datastrm)
{
	std::vector<int> res{};
	std::string tmp{};

	for (;;) {
		int n;
		
		datastrm >> n;

		if (datastrm.eof()) {
			break;
		}

		res.push_back(n);
	}

	return res;
}

template <typename Func>
std::size_t escape(std::vector<int> const& elems_orig, Func f)
{	
	std::size_t n = 0;
	std::size_t i = 0;

	// Local copy
	std::vector<int> elems = elems_orig;

	while (i < elems.size()) {
		auto next_i = elems[i];

		elems[i] = f(elems[i]);
		++n;

		i += next_i;
	}

	return n;
}

int main(int argc, char* argv[])
{
	auto elems = read_input(std::cin);

	auto part1 = escape(elems, [] (int n) { return n + 1; });
	auto part2 = escape(elems, [] (int n) { return n > 2 ? (n - 1) : (n + 1); });

	std::cout << "Part 1: " << part1 << std::endl;
	std::cout << "Part 2: " << part2 << std::endl;
}
