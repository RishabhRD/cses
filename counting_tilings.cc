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

ll add_mask(ll n, ll i) { return n | (1 << i); }

ll remove_mask(ll n, ll i) { return (n & ~(1 << i)); }

bool has_mask(ll n, ll i) { return n & (1 << i); }

auto generate_next_masks(ll i, ll cur_mask, ll m, ll prev_mask,
                         std::vector<ll> &res) {
  if (i == m) {
    res.push_back(cur_mask);
    return;
  }
  if (has_mask(prev_mask, i)) {
    generate_next_masks(i + 1, cur_mask, m, prev_mask, res);
  } else {
    generate_next_masks(i + 1, add_mask(cur_mask, i), m, prev_mask, res);
    if (i + 1 < m && !has_mask(prev_mask, i + 1)) {
      generate_next_masks(i + 2, cur_mask, m, prev_mask, res);
    }
  }
}

auto generate_next_masks(ll prev_mask, ll m) {
  std::vector<ll> next_masks;
  generate_next_masks(0, 0, m, prev_mask, next_masks);
  return next_masks;
}

ll dfs(ll const i, ll const cur_mask, ll m, ll n,
       std::vector<std::vector<ll>> &dp) {
  if (i == n) {
    return cur_mask == 0 ? 1 : 0;
  }
  if (dp[i][cur_mask] != -1)
    return dp[i][cur_mask];
  auto const next_masks = generate_next_masks(cur_mask, m);
  ll ans = 0;
  for (auto const next_mask : next_masks) {
    ans = (ans + dfs(i + 1, next_mask, m, n, dp)) % mod;
  }
  return dp[i][cur_mask] = ans;
}

auto solve(ll m, ll n) {
  std::vector dp(n, std::vector(1 << m, -1ll));
  return dfs(0, 0, m, n, dp);
}

int main() {
  auto const m = read<ll>();
  auto const n = read<ll>();
  std::cout << solve(m, n) << std::endl;
}
