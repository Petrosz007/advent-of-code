using System.Text.RegularExpressions;
using System.Diagnostics;
using System.Collections.Generic;
using System.Linq;
using System;
using System.IO;

using Graph = System.Collections.Generic.Dictionary<string, System.Collections.Generic.List<(string, int)>>;

namespace Day7cs
{
    public class Program
    {   
        public static List<(string, int)> ParseContainedBags(string line) =>
            new Regex(@"(?<count>\d+) (?<type>\S+ \S+) bag")
                .Matches(line)
                .Select(match => (match.Groups["type"].Value, int.Parse(match.Groups["count"].Value)))
                .ToList();

        public static (string, List<(string, int)>) ParseLine(string line)
        {
            var matchNoBags = new Regex(@"^(?<container>\S+ \S+) bags contain no").Match(line);
            if(matchNoBags.Success)
            {
                return (matchNoBags.Groups["container"].Value, new ());
            }

            var matchBags = new Regex(@"^(?<container>\S+ \S+) bags contain (?<bags>(\d+ \S+ \S+ bag(s)?(, )?)+).$").Match(line);
            var container = matchBags.Groups["container"].Value;
            var containedBags = ParseContainedBags(matchBags.Groups["bags"].Value);
            return (container, containedBags);
        }

        public static Graph ParseToGraph(string[] lines)
        {
            var graph = new Graph();
            var parsedLines = lines.Select(ParseLine);
            foreach(var line in parsedLines)
            {
                var container = line.Item1;
                var contained = line.Item2;
                graph.Add(container, contained);
            }

            return graph;
        }

        public static bool BFSFind(string node, Graph graph, string target) =>
            node.Equals(target)
                ? true
                : graph[node]
                    .Select(x => x.Item1)
                    .Any(x => BFSFind(x, graph, target));

        public static int Solve1(Graph graph, string target = "shiny gold") =>
            graph.Keys
                .Where(node => BFSFind(node, graph, target))
                .Count() - 1;

        public static int BFSSum(string node, Graph graph) =>
            graph[node].Count == 0
                ? 0
                : graph[node].Sum(x => (1 + BFSSum(x.Item1, graph)) * x.Item2);
        
        public static int Solve2(Graph graph, string target = "shiny gold") =>
            BFSSum(target, graph);

        public static void Main(string[] args)
        {
            var input = File.ReadAllLines("input.txt");

            var stopwatch = Stopwatch.StartNew();

            var graph = ParseToGraph(input);

            var part1 = Solve1(graph);
            var part2 = Solve2(graph);

            stopwatch.Stop();

            Console.WriteLine($"Done in {stopwatch.Elapsed.TotalMilliseconds}ms");
            Console.WriteLine($"Part1: {part1}");
            Console.WriteLine($"Part2: {part2}");
        }
    }
}
