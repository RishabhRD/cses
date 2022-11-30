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

ll num_n_digits(std::vector<ll> const &digits, ll i, bool tight, ll prev,
                bool leading,
                std::vector<std::vector<std::vector<std::vector<ll>>>> &dp) {
  if (i > std::size(digits))
    return 1;
  if (dp[i][prev][leading][tight] != -1)
    return dp[i][prev][leading][tight];
  ll ans = 0;
  if (tight) {
    for (ll j = 0; j <= digits[i - 1]; ++j) {
      bool z = leading && j == 0;
      bool cur_tight = j == digits[i - 1] ? true : false;
      if (prev != j || z) {
        ans += num_n_digits(digits, i + 1, cur_tight, j, z, dp);
      }
    }
  } else {
    for (ll j = 0; j <= 9; ++j) {
      bool z = leading && j == 0;
      if (prev != j || z) {
        ans += num_n_digits(digits, i + 1, false, j, z, dp);
      }
    }
  }
  return dp[i][prev][leading][tight] = ans;
}

auto digits(ll n) {
  std::vector<ll> digits;
  while (n) {
    digits.push_back(n % 10);
    n = n / 10;
  }
  std::reverse(std::begin(digits), std::end(digits));
  return digits;
}

auto num_n_digits(ll n) {
  if (n == 0)
    return 1ll;
  auto const digits_vec = digits(n);
  auto const num_digits = std::size(digits_vec);
  std::vector dp(num_digits + 1,
                 std::vector(11, std::vector(2, std::vector(2, -1ll))));
  return num_n_digits(digits_vec, 1, true, 10, true, dp);
}

int main() {
  auto const a = read<ll>();
  auto const b = read<ll>();
  std::cout << num_n_digits(b) - num_n_digits(a - 1) << std::endl;
}
