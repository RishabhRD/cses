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

auto solve(std::vector<std::pair<ll, ll>> &movies, ll k) {
  std::sort(std::begin(movies), std::end(movies));
  std::vector<std::pair<ll, bool>> events;
  for (auto const [start, end] : movies) {
    events.push_back({start, true});
    events.push_back({end, false});
  }
  for (auto [time, isArrival] : events) {
    std::cout << time << ' ' << (isArrival ? "Arrival" : "Leave") << std::endl;
  }
  ll cur_avail = k;
  ll cur_watched = 0;
  for (auto const [_, isArrival] : events) {
    if (!isArrival) {
      ++cur_avail;
    } else {
      if (cur_avail > 0) {
        ++cur_watched;
      }
      --cur_avail;
    }
  }
  return cur_watched;
}

int main() {
  auto const n = read<ll>();
  auto const k = read<ll>();
  std::vector<std::pair<ll, ll>> movies;
  for (ll i = 0; i < n; ++i) {
    auto const start = read<ll>();
    auto const end = read<ll>();
    movies.push_back({start, end});
  }
  std::cout << solve(movies, k) << std::endl;
}
