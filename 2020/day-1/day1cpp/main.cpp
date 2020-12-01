#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>
#include <chrono>
#include <tuple>
#include <optional>

std::optional<std::tuple<int,int>> search_pair(
    const std::vector<int>::const_iterator& nums_begin, 
    const std::vector<int>::const_iterator& nums_end, 
    int target
)
{
    for(auto it = nums_begin; it != nums_end; ++it) {
        auto value = target - *it;
        if(value < 0) return {};
        if(std::binary_search(it, nums_end, value))
        {
            return std::make_tuple(*it, value);
        }
    }
    return {};
}

int solve1(const std::vector<int>& nums)
{
    auto result = search_pair(std::begin(nums), std::end(nums), 2020).value();
    return std::get<0>(result) * std::get<1>(result);
}

int solve2(const std::vector<int>& nums)
{
    for(auto it = std::begin(nums); it != std::end(nums); ++it)
    {
        auto target = 2020 - *it;
        auto result = search_pair(it, std::end(nums), target);
        if(result.has_value())
        {
            return std::get<0>(result.value()) * std::get<1>(result.value()) * (*it);
        }
    }
}

int main()
{
    std::vector<int> nums;
    std::ifstream file("input.txt");

    int data;
    while(file >> data) {
        nums.push_back(data);
    }

    std::stable_sort(std::begin(nums), std::end(nums));

    auto start = std::chrono::high_resolution_clock::now();

    auto part1 = solve1(nums);
    auto part2 = solve2(nums);

    auto end = std::chrono::high_resolution_clock::now();
	std::chrono::duration<float, std::milli> duration = end - start;

    std::cout << "Part 1: " << part1 << std::endl
              << "Part 2: " << part2 << std::endl
              << "Duration: " << duration.count() << "ms" << std::endl;
    return 0;
}