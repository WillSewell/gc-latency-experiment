namespace gc_latency_experiment
{
    using System;
    using System.Collections.Generic;
    using System.Diagnostics;
    using System.Globalization;
    using System.Linq;
    class Program
    {
        private const int windowSize = 200000;
        private const int msgCount = 10000000;
        private const int msgSize = 1024;

        private static byte[] createMessage(int n)
        {
            return Enumerable.Repeat((byte)n, msgSize).ToArray();
        }

        private static void pushMessage(Dictionary<int, byte[]> map, int id)
        {
            var lowId = id - windowSize;
            map.Add(id, createMessage(id));
            if (lowId >= 0)
            {
                map.Remove(lowId);
            }
        }

        static void Main(string[] args)
        {
            var worst = new TimeSpan();
            var map = new Dictionary<int, byte[]>();
            for (var i = 0; i < msgCount; i++)
            {
                var sw = Stopwatch.StartNew();
                pushMessage(map, i);
                sw.Stop();
                if (sw.Elapsed > worst)
                {
                    worst = sw.Elapsed;
                }
            }

            Console.WriteLine($"Worst push time : {worst.TotalMilliseconds.ToString(CultureInfo.InvariantCulture)} ms");
        }
    }
}