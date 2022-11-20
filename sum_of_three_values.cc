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

std::optional<std::tuple<ll, ll, ll>> solve(std::vector<ll> &nums_, ll x) {
  auto const n = std::size(nums_);
  std::vector<std::pair<ll, ll>> indexed;
  for (ll i = 0; i < n; ++i) {
    indexed.push_back({nums_[i], i});
  }
  std::sort(std::begin(indexed), std::end(indexed));
  std::vector<ll> nums;
  std::vector<ll> indexes;
  for (ll i = 0; i < n; ++i) {
    nums.push_back(indexed[i].first);
    indexes.push_back(indexed[i].second);
  }
  std::sort(std::begin(nums), std::end(nums));
  for (int i = 0; i < n; ++i) {
    for (int j = i + 1; j < n; ++j) {
      auto const target = x - (nums[i] + nums[j]);
      auto const itr =
          std::lower_bound(std::begin(nums) + j + 1, std::end(nums), target);
      if (itr != std::end(nums) && *itr == target) {
        auto const k = itr - std::begin(nums);
        return std::tuple{indexes[i], indexes[j], indexes[k]};
      }
    }
  }
  return std::nullopt;
}

int main() {
  auto const n = read<ll>();
  auto const x = read<ll>();
  auto nums = read_vec<ll>(n);
  auto const ans = solve(nums, x);
  if (ans.has_value()) {
    auto const [i, j, k] = ans.value();
    std::cout << i + 1 << ' ' << j + 1 << ' ' << k + 1 << std::endl;
  } else {
    std::cout << "IMPOSSIBLE" << std::endl;
  }
}
