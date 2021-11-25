namespace Monorepo2020.Solutions;

public class Day1 : SolutionBase<List<long>>
{
    public override List<long> ParseInput(IEnumerable<string> input) =>
        input.Select(long.Parse).ToList();
    
    public override long Solve1(List<long> nums)
    {
        for(int i = 0; i < nums.Count; ++i)
            for(int j = i+1; j < nums.Count; ++j)
                if(nums[i]+nums[j] == 2020)
                    return nums[i]*nums[j];

        return -1;
    }

    public override long Solve2(List<long> nums)
    {
        for(int i = 0; i < nums.Count; ++i)
            for(int j = i+1; j < nums.Count; ++j)
                for(int k = j+1; k < nums.Count; ++k)
                    if(nums[i]+nums[j]+nums[k] == 2020)
                        return nums[i]*nums[j]*nums[k];

        return -1;
    }
}