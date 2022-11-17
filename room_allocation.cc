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

struct event {
  int time;
  bool is_arrival;
  int idx;
};

auto solve(std::vector<std::pair<int, int>> const &nums) {
  auto const n = std::size(nums);
  std::vector<event> events;
  for (int i = 0; i < n; ++i) {
    events.push_back({nums[i].first, true, i});
    events.push_back({nums[i].second, false, i});
  }
  std::sort(std::begin(events), std::end(events), [](auto a, auto b) {
    if (a.time == b.time)
      return a.is_arrival;
    return a.time < b.time;
  });
  std::vector<int> ans(n);
  std::vector<int> available_rooms;
  int cur = 0;
  for (auto [time, is_arrival, idx] : events) {
    if (is_arrival) {
      if (available_rooms.empty()) {
        ++cur;
        ans[idx] = cur;
      } else {
        ans[idx] = available_rooms.back();
        available_rooms.pop_back();
      }
    } else {
      available_rooms.push_back(ans[idx]);
    }
  }
  auto const max = *std::max_element(std::begin(ans), std::end(ans));
  std::cout << max << std::endl;
  for (auto ele : ans)
    std::cout << ele << ' ';
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
