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

std::optional<std::tuple<ll, ll, ll, ll>> solve(std::vector<ll> &nums, int k) {
  std::unordered_map<ll, std::pair<ll, ll>> mp;
  auto const n = std::size(nums);
  for (int i = 1; i < n; ++i) {
    for (int j = 0; j < i - 1; ++j) {
      mp[nums[i - 1] + nums[j]] = {i - 1, j};
    }
    for (int j = i + 1; j < n; ++j) {
      auto const sum = nums[i] + nums[j];
      auto const target = k - sum;
      if (mp.count(target)) {
        return std::tuple{i, j, mp[target].first, mp[target].second};
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
    auto const [i, j, k, l] = ans.value();
    std::cout << i + 1 << ' ' << j + 1 << ' ' << k + 1 << ' ' << l + 1
              << std::endl;
  } else {
    std::cout << "IMPOSSIBLE" << std::endl;
  }
}
