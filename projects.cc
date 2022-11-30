#include <algorithm>
#include <array>
#include <bitset>
#include <chrono>
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

struct custom_hash {
  static uint64_t splitmix64(uint64_t x) {
    x += 0x9e3779b97f4a7c15;
    x = (x ^ (x >> 30)) * 0xbf58476d1ce4e5b9;
    x = (x ^ (x >> 27)) * 0x94d049bb133111eb;
    return x ^ (x >> 31);
  }

  size_t operator()(uint64_t x) const {
    static const uint64_t FIXED_RANDOM =
        std::chrono::steady_clock::now().time_since_epoch().count();
    return splitmix64(x + FIXED_RANDOM);
  }
};

using ll = long long;

using safe_set = std::unordered_set<ll, custom_hash>;

template <typename T> using safe_map = std::unordered_map<ll, T>;

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

template <typename T> auto read_matrix(int m, int n) {
  std::vector<std::vector<T>> vec(m, std::vector<T>(n));
  for (int i = 0; i < m; ++i) {
    for (int j = 0; j < n; ++j) {
      std::cin >> vec[i][j];
    }
  }
  return vec;
}

struct entry_t {
  ll start;
  ll end;
  ll award;
};

auto solve(std::vector<entry_t> &nums) {
  std::sort(std::begin(nums), std::end(nums),
            [](auto a, auto b) { return a.end < b.end; });
  std::vector<ll> ends;
  for (auto [_, end, __] : nums) {
    ends.push_back(end);
  }
  std::reverse(std::begin(ends), std::end(ends));
  auto const n = std::size(nums);
  std::vector dp(n + 1, 0ll);
  for (int i = 1; i <= n; ++i) {
    auto const [start, _, award] = nums[i - 1];
    auto const idx = n - (std::upper_bound(std::begin(ends), std::end(ends),
                                           start, std::greater<>{}) -
                          std::begin(ends));
    dp[i] = std::max(dp[i - 1], nums[i - 1].award + dp[idx]);
  }
  return *std::max_element(std::begin(dp), std::end(dp));
}

int main() {
  auto const n = read<ll>();
  std::vector<entry_t> nums(n);
  for (auto &e : nums) {
    std::cin >> e.start >> e.end >> e.award;
  }
  std::cout << solve(nums) << std::endl;
}
