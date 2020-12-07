using System.Text.RegularExpressions;
using System.Diagnostics;
using System.Collections.Generic;
using System.Linq;
using System;
using System.IO;

using Edges = System.Collections.Generic.List<(string Parent, string Child, int Count)>;
using Tree = System.Linq.ILookup<string, (string Name, int Count)>;

namespace Day7cs
{
    public class Program
    {   
        public static Edges ParseContainedBags(string parent, string line) =>
            new Regex(@"(?<count>\d+) (?<type>\S+ \S+) bag")
                .Matches(line)
                .Select(match => (parent, match.Groups["type"].Value, int.Parse(match.Groups["count"].Value)))
                .ToList();

        public static Edges ParseLine(string line)
        {
            var matchNoBags = new Regex(@"^(?<container>\S+ \S+) bags contain no").Match(line);
            if(matchNoBags.Success)
                return new ();

            var matchBags = new Regex(@"^(?<container>\S+ \S+) bags contain (?<bags>(\d+ \S+ \S+ bag(s)?(, )?)+).$").Match(line);
            var container = matchBags.Groups["container"].Value;
            var containedBags = ParseContainedBags(container, matchBags.Groups["bags"].Value);

            return containedBags;
        }

        public static Tree ParseToTree(string[] lines) =>
            lines
                .SelectMany(ParseLine)
                .ToLookup(x => x.Parent, x => (x.Child, x.Count));


        public static bool BFSFind(string node, Tree tree, string target) =>
            node.Equals(target)
                ? true
                : tree[node]
                    .Select(x => x.Name)
                    .Any(x => BFSFind(x, tree, target));

        public static int Solve1(Tree tree, string target = "shiny gold") =>
            tree
                .Select(edge => edge.Key)
                .Where(node => BFSFind(node, tree, target))
                .Count() - 1;

        public static int BFSSum(string node, Tree tree) =>
            tree[node].Count() == 0
                ? 0
                : tree[node].Sum(x => (1 + BFSSum(x.Name, tree)) * x.Count);
        
        public static int Solve2(Tree tree, string target = "shiny gold") =>
            BFSSum(target, tree);

        public static void Main(string[] args)
        {
            var input = File.ReadAllLines("input.txt");

            var stopwatch = Stopwatch.StartNew();

            var tree = ParseToTree(input);

            var part1 = Solve1(tree);
            var part2 = Solve2(tree);

            stopwatch.Stop();

            Console.WriteLine($"Done in {stopwatch.Elapsed.TotalMilliseconds}ms");
            Console.WriteLine($"Part1: {part1}");
            Console.WriteLine($"Part2: {part2}");
        }
    }
}
