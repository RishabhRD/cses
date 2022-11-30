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

constexpr ll mod = 1e9 + 7;

ll dfs(std::vector<ll> const &nums, ll i, ll from, ll m,
       std::vector<std::vector<ll>> &dp) {
  if (i == std::size(nums))
    return 1;
  if (dp[from][i] != -1)
    return dp[from][i];
  ll ans = 0;
  if (nums[i] == 0) {
    ans = (ans + dfs(nums, i + 1, from, m, dp)) % mod;
    if (from - 1 >= 1)
      ans = (ans + dfs(nums, i + 1, from - 1, m, dp)) % mod;
    if (from + 1 <= m)
      ans = (ans + dfs(nums, i + 1, from + 1, m, dp)) % mod;
  } else {
    if (std::abs(nums[i] - from) > 1) {
      return 0;
    } else {
      ans = (ans + dfs(nums, i + 1, nums[i], m, dp)) % mod;
    }
  }
  return dp[from][i] = ans;
}

auto solve(std::vector<ll> const &nums, ll m) {
  auto const n = std::size(nums);
  std::vector dp(m + 1, std::vector(n, ll(-1)));
  if (nums[0] == 0) {
    ll ans = 0;
    for (int i = 1; i <= m; ++i) {
      ans = (ans + dfs(nums, 1, i, m, dp)) % mod;
    }
    return ans;
  } else {
    return dfs(nums, 1, nums[0], m, dp);
  }
}

int main() {
  auto const n = read<ll>();
  auto const m = read<ll>();
  auto const nums = read_vec<ll>(n);
  std::cout << solve(nums, m) << std::endl;
}
