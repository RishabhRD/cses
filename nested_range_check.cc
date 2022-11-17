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

struct range {
  int low;
  int high;
  int idx;
};

struct ans_t {
  bool ans;
  int idx;
};

using ll = long long;

auto solve(std::vector<std::pair<int, int>> const &ranges_) {
  auto const n = std::size(ranges_);
  std::vector<range> ranges(n);
  for (int i = 0; i < n; ++i) {
    ranges[i] = {ranges_[i].first, ranges_[i].second, i};
  }
  std::sort(std::begin(ranges), std::end(ranges), [](auto a, auto b) {
    if (a.low == b.low)
      return a.high > b.high;
    return a.low < b.low;
  });
  std::vector<ans_t> second_ans;
  int max = -1;
  for (int i = 0; i < n; ++i) {
    auto const isContained = ranges[i].high <= max;
    second_ans.push_back({isContained, ranges[i].idx});
    max = std::max(max, ranges[i].high);
  }
  std::vector<ans_t> first_ans;
  int min = INT32_MAX;
  for (int i = n - 1; i >= 0; --i) {
    auto const isContained = ranges[i].high >= min;
    first_ans.push_back({isContained, ranges[i].idx});
    min = std::min(min, ranges[i].high);
  }
  std::sort(std::begin(first_ans), std::end(first_ans),
            [](auto a, auto b) { return a.idx < b.idx; });
  std::sort(std::begin(second_ans), std::end(second_ans),
            [](auto a, auto b) { return a.idx < b.idx; });
  for (auto [e, _] : first_ans) {
    std::cout << e << ' ';
  }
  std::cout << std::endl;
  for (auto [e, _] : second_ans) {
    std::cout << e << ' ';
  }
  std::cout << std::endl;
}

int main() {
  auto const n = read<int>();
  std::vector<std::pair<int, int>> nums;
  for (int i = 0; i < n; ++i) {
    auto const a = read<int>();
    auto const b = read<int>();
    nums.push_back({a, b});
  }
  solve(nums);
}
