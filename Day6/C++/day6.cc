#include <iostream>
#include <vector>
#include <unordered_map>

class crc32 {
public:
	crc32()
	{
		for (uint32_t i = 0; i < 256; ++i) {
			uint32_t v = i;

			for (uint32_t j = 0; j < 8; ++j) {
				if (v & 1) {
					v = 0xedb99320 ^ (v >> 1);
				} else {
					v >>= 1;
				}
			}

			_ltab[i] = v;
		}
	}

	void reset()
	{
		_digest = 0xffffffff;
	}

	void update(uint8_t c)
	{
		_digest = _ltab[(_digest ^ c) & 0xff] ^ (_digest >> 8);
	}

	uint32_t result()
	{
		return _digest ^ 0xffffffff;
	}

private:
	uint32_t _digest;
	uint32_t _ltab[256];
};

static crc32 crc{};

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

std::size_t find_max(std::vector<int> const& elems)
{
	std::size_t max = 0;

	for (std::size_t i = 0; i < elems.size(); ++i) {
		if (elems[i] > elems[max]) {
			max = i;
		}
	}

	return max;
}

std::size_t distribute(std::vector<int>& elems)
{	
	std::size_t i = find_max(elems);
	std::size_t n = elems[i];

	elems[i++] = 0;

	while (n--) {
		++elems[i++ % elems.size()];
	}

	return i;
}

uint64_t checksum(std::vector<int> const& elems)
{
	crc.reset();

	for (auto i : elems) {
		crc.update(i);
	}

	return crc.result();
}

void dump(std::vector<int> const& elems)
{
	for (auto e : elems) {
		std::cout << e << '\t';
	}

	std::cout << std::endl;
}

int main(int argc, char* argv[])
{
	auto elems = read_input(std::cin);

	std::unordered_map<uint64_t, std::size_t> checksums{};

	std::size_t n = 0;

	for (;;) {
		auto chk = checksum(elems);
		auto pos = checksums.find(chk);

		if (pos != std::end(checksums)) {
			std::cout << "Found " << chk << " again after " << (n - pos->second) << std::endl;
			break;

		} else {
			checksums[chk] = n;
		}

		distribute(elems);
		++n;
	}

	std::cout << "Took " << n << " shuffles to finish" << std::endl;
}
