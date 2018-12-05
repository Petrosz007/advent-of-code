#include <iostream>
#include <string>
#include <vector>
#include <algorithm>


int main() {
	int c2 = 0, c3 = 0;
	std::string str;
	std::vector<std::string> vec;
	while (std::cin >> str) {
		vec.push_back(str);
		if (vec.size() == 250) break;
	}
	std::cout << "break: " << c2++ << "\n";
	std::sort(vec.begin(), vec.end());
	for (int i = 0; i < vec.size() - 1; ++i) {
		int count = 0;
		auto str1 = vec[i].c_str();
		auto str2 = vec[i + 1].c_str();
		while (*str1 && *str2) {
			if (*str1++ != *str2++) count++;
			if (count > 1) break;
		}
		if (count == 1) {
			str1 = vec[i].c_str();
			str2 = vec[i + 1].c_str();
			std::string s;
			while (*str1 && *str2) {
				if (*str1 == *str2) {
					s += *str1;
				}
				str1++, str2++;
			}
			s += *str1;
			std::cout << "alma\n" << vec[i] << "\n" << vec[i + 1] << "\n" << s << "\n\n";
		}
	}

	while (std::cin.get());
}