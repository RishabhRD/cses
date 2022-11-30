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

auto solve(ll A, ll B) {
  std::vector dp(A + 1, std::vector(B + 1, -1ll));
  for (ll i = 1; i <= A; ++i) {
    dp[i][1] = i - 1;
  }
  for (ll j = 1; j <= B; ++j) {
    dp[1][j] = j - 1;
  }
  for (ll a = 2; a <= A; ++a) {
    for (ll b = 2; b <= B; ++b) {
      if (a == b) {
        dp[a][b] = 0;
      } else {
        ll min = INT32_MAX;
        for (ll i = 1; i < a; ++i) {
          min = std::min(min, dp[i][b] + dp[a - i][b]);
        }
        for (ll i = 1; i < b; ++i) {
          min = std::min(min, dp[a][i] + dp[a][b - i]);
        }
        dp[a][b] = 1 + min;
      }
    }
  }
  return dp[A][B];
}

int main() {
  auto const a = read<ll>();
  auto const b = read<ll>();
  std::cout << solve(a, b) << std::endl;
}
