using System.Text.RegularExpressions;

namespace Monorepo2020.Solutions;

using Edges = List<(string Parent, string Child, int Count)>;
using Tree = ILookup<string, (string Name, int Count)>;

public class Day7 : SolutionBase<Tree>
{
    public Edges ParseContainedBags(string parent, string line) =>
        new Regex(@"(?<count>\d+) (?<type>\S+ \S+) bag")
            .Matches(line)
            .Select(match => (parent, match.Groups["type"].Value, int.Parse(match.Groups["count"].Value)))
            .ToList();
    
    public Edges ParseLine(string line)
    {
        var matchNoBags = new Regex(@"^(?<container>\S+ \S+) bags contain no").Match(line);
        if(matchNoBags.Success)
            return new ();

        var matchBags = new Regex(@"^(?<container>\S+ \S+) bags contain (?<bags>(\d+ \S+ \S+ bag(s)?(, )?)+).$").Match(line);
        var container = matchBags.Groups["container"].Value;
        var containedBags = ParseContainedBags(container, matchBags.Groups["bags"].Value);

        return containedBags;
    }
    
    public override Tree ParseInput(IEnumerable<string> input) =>
        input.SelectMany(ParseLine)
            .ToLookup(x => x.Parent, x => (x.Child, x.Count));
    
    public bool BFSFind(string node, Tree tree, string target) =>
        node.Equals(target) || 
            tree[node]
                .Select(x => x.Name)
                .Any(x => BFSFind(x, tree, target));
    
    public override long Solve1(Tree tree) =>
        tree
            .Select(edge => edge.Key)
            .Count(node => BFSFind(node, tree, "shiny gold")) - 1;
    
    public int BFSSum(string node, Tree tree) =>
        !tree[node].Any()
            ? 0
            : tree[node].Sum(x => (1 + BFSSum(x.Name, tree)) * x.Count);
    public override long Solve2(Tree tree) =>
        BFSSum("shiny gold", tree);
}