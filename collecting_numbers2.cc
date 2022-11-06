#include <algorithm>
#include <array>
#include <bitset>
#include <cmath>
#include <deque>
#include <iostream>
#include <iterator>
#include <limits>
#include <map>
#include <numeric>
#include <optional>
#include <queue>
#include <set>
#include <stack>
#include <unordered_map>
#include <unordered_set>
#include <vector>

template <typename T> T read() {
  T t;
  std::cin >> t;
  return t;
}

template <typename T> std::vector<T> read_vec(int n) {
  std::vector<T> vec(n);
  for (auto &ele : vec)
    std::cin >> ele;
  return vec;
}

template <typename T> std::vector<T> read_matrix(int m, int n) {
  std::vector<std::vector<T>> vec(m, std::vector<T>(n));
  for (int i = 0; i < m; ++i) {
    for (int j = 0; j < n; ++j) {
      std::cin >> vec[i][j];
    }
  }
  return vec;
}

using ll = long long;

auto solve(std::vector<int> &nums,
           std::vector<std::pair<int, int>> const &queries) {
  auto val = [](auto a, auto b) {
    if (a && !b)
      return -1;
    if (!a and b)
      return 1;
    return 0;
  };
  auto const n = std::size(nums);
  std::vector<int> pos(n + 1);
  pos[0] = n;
  for (int i = 0; i < n; ++i) {
    pos[nums[i]] = i;
  }
  auto is_valid = [&pos](auto x) { return pos[x] > pos[x - 1]; };
  int initVal = 0;
  for (auto ele : nums) {
    if (is_valid(ele))
      ++initVal;
  }
  int curVal = initVal;
  std::cout << n << ' ' << curVal << std::endl;
  for (auto [x, y] : queries) {
    x = nums[x - 1];
    y = nums[y - 1];
    auto const xBefore = is_valid(x);
    auto const yBefore = is_valid(y);
    std::swap(pos[x], pos[y]);
    auto const xAfter = is_valid(x);
    auto const yAfter = is_valid(y);
    std::cout << x << ' ' << y << ": " << xBefore << ' ' << xAfter << ' '
              << yBefore << ' ' << yAfter << std::endl;
    curVal += val(xBefore, xAfter) + val(yBefore, yAfter);
    std::cout << n - curVal << std::endl;
  }
}

int main() {
  auto const n = read<int>();
  auto const m = read<int>();
  auto nums = read_vec<int>(n);
  std::vector<std::pair<int, int>> queries;
  for (int i = 0; i < m; ++i) {
    auto const x = read<int>();
    auto const y = read<int>();
    queries.push_back({x, y});
  }
  solve(nums, queries);
}
