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

constexpr ll mod = 1e9 + 7;

void solve(std::vector<ll> const &coins, ll n, std::set<std::vector<ll>> &res,
           std::vector<ll> &dp) {
  if (n < 0)
    return 0;
  else if (n == 0)
    return 1;
  else {
    if (dp[n] != -1)
      return dp[n];
    ll ans = 0;
    for (auto coin : coins) {
      ans = (ans + solve(coins, n - coin, dp)) % mod;
    }
    return dp[n] = ans;
  }
}

ll solve(std::vector<ll> const &coins, ll n) {
  std::vector<ll> dp(n + 1, -1);
  return solve(coins, n, dp);
}

int main() {
  auto const n = read<ll>();
  auto const k = read<ll>();
  auto const coins = read_vec<ll>(n);
  std::cout << solve(coins, k) << std::endl;
}
