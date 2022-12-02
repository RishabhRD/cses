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

auto find(std::vector<std::string> const &graph, char c) {
  auto const m = std::size(graph);
  auto const n = std::size(graph[0]);
  std::vector<std::pair<ll, ll>> res;
  for (ll i = 0; i < m; ++i) {
    for (ll j = 0; j < n; ++j) {
      if (graph[i][j] == c)
        res.push_back({i, j});
    }
  }
  return res;
}

auto solve(std::vector<std::string> const &graph) {
  auto const m = std::size(graph);
  auto const n = std::size(graph[0]);
  auto const is_boundary = [&](ll x, ll y) {
    return (x == 0 || y == 0 || x == m - 1 || y == n - 1);
  };
  auto const reach_boundary = [&](auto const &vec) {
    ll curTime = 0;
    std::queue<std::pair<ll, ll>> q;
    std::vector from(m, std::vector(n, std::pair{-1ll, -1ll}));
    std::vector time(m, std::vector(n, -1ll));
    for (auto [x, y] : vec) {
      q.push({x, y});
      from[x][y] = std::pair{x, y};
      time[x][y] = 0;
    }
    while (not std::empty(q)) {
      auto sz = q.size();
      ++curTime;
      while (sz--) {
        auto const [x, y] = q.front();
        q.pop();
        constexpr static ll dx[] = {-1, 1, 0, 0};
        constexpr static ll dy[] = {0, 0, -1, 1};
        for (ll i = 0; i < 4; ++i) {
          auto const nx = x + dx[i];
          auto const ny = y + dy[i];
          if (nx >= 0 and ny >= 0 and nx < m and ny < n and
              graph[nx][ny] != '#' && from[nx][ny] == std::pair{-1ll, -1ll}) {
            q.push({nx, ny});
            from[nx][ny] = {x, y};
            time[nx][ny] = curTime;
          }
        }
      }
    }
    return std::pair{from, time};
  };
  auto const heroPosition = find(graph, 'A');
  auto const monsterPositions = find(graph, 'M');
  auto const [hFrom_, hTime_] = reach_boundary(heroPosition);
  auto const [mFrom, mTime] = reach_boundary(monsterPositions);
  auto const hTime = std::move(hTime_);
  auto const hFrom = std::move(hFrom_);
  auto const retrace = [&](ll i, ll j) {
    std::cout << "YES" << std::endl;
    std::cout << hTime[i][j] << std::endl;
    std::string path;
    auto prev = std::pair{i, j};
    auto cur = std::pair{i, j};
    while (cur != std::pair{heroPosition[0].first, heroPosition[0].second}) {
      prev = cur;
      cur = hFrom[cur.first][cur.second];
      if (prev.first < cur.first) {
        path.push_back('U');
      } else if (prev.first > cur.first) {
        path.push_back('D');
      } else if (prev.second > cur.second) {
        path.push_back('R');
      } else {
        path.push_back('L');
      }
    }
    std::reverse(std::begin(path), std::end(path));
    std::cout << path << std::endl;
  };
  for (ll i = 0; i < m; ++i) {
    for (ll j = 0; j < n; ++j) {
      if (is_boundary(i, j)) {
        if (hTime[i][j] == -1) {
          continue;
        } else if (mTime[i][j] == -1) {
          retrace(i, j);
          return;
        }
        if (hTime[i][j] < mTime[i][j]) {
          retrace(i, j);
          return;
        }
      }
    }
  }
  std::cout << "NO" << std::endl;
}

int main() {
  auto const m = read<ll>();
  auto const n = read<ll>();
  std::vector<std::string> graph;
  for (ll i = 0; i < m; ++i) {
    graph.push_back(read<std::string>());
  }
  solve(graph);
}
