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
constexpr ll mod = 1e9 + 7;

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

auto const find(std::vector<std::string> const &graph, char c) {
  auto const m = std::size(graph);
  auto const n = std::size(graph[0]);
  for (ll i = 0; i < m; ++i) {
    for (ll j = 0; j < n; ++j) {
      if (graph[i][j] == c)
        return std::pair{i, j};
    }
  }
  return std::pair<ll, ll>{0, 0};
}

auto solve(std::vector<std::string> const &graph) {
  auto const m = std::size(graph);
  auto const n = std::size(graph[0]);
  auto const [xfinal, yfinal] = find(graph, 'A');
  auto const [xinit, yinit] = find(graph, 'B');
  constexpr static ll dx[] = {0, 1, -1, 0};
  constexpr static ll dy[] = {1, 0, 0, -1};
  ll ans = -1;
  std::vector from(m, std::vector(n, std::pair{-1ll, -1ll}));
  std::queue<std::pair<ll, ll>> q;
  q.push({xinit, yinit});
  from[xinit][yinit] = {xinit, yinit};
  [&](auto xfinal, auto yfinal) {
    while (not std::empty(q)) {
      auto sz = q.size();
      ++ans;
      while (sz--) {
        auto const [i, j] = q.front();
        if (i == xfinal and j == yfinal)
          return;
        q.pop();
        for (ll k = 0; k < 4; ++k) {
          auto const nx = i + dx[k];
          auto const ny = j + dy[k];
          if (nx >= 0 and ny >= 0 and nx < std::size(graph) and
              ny < std::size(graph[0]) and graph[nx][ny] != '#') {
            auto [fx, fy] = from[nx][ny];
            if (from[nx][ny] == std::pair{-1ll, -1ll}) {
              from[nx][ny] = std::pair{i, j};
              q.push({nx, ny});
            }
          }
        }
      }
    }
  }(xfinal, yfinal);
  if (from[xfinal][yfinal] == std::pair{-1ll, -1ll}) {
    std::cout << "NO" << std::endl;
  } else {
    std::cout << "YES" << std::endl;
    std::cout << ans << std::endl;
    std::pair prev{xfinal, yfinal};
    std::pair cur{xfinal, yfinal};
    while (cur != std::pair{xinit, yinit}) {
      prev = cur;
      cur = from[cur.first][cur.second];
      if (cur.first > prev.first) {
        std::cout << "D";
      } else if (cur.first < prev.first) {
        std::cout << "U";
      } else if (cur.second > prev.second) {
        std::cout << "R";
      } else {
        std::cout << "L";
      }
    }
    std::cout << std::endl;
  }
}

int main() {
  auto const n = read<ll>();
  auto const m = read<ll>();
  auto const graph = read_vec<std::string>(n);
  solve(graph);
}
