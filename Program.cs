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

        private static byte[][] array = new byte[msgCount][];

        private static byte[] createMessage(int n)
        {
            var a = new byte[msgSize];
            for (var i = 0; i < a.Length; i++)
            {
                a[i] = (byte)n;
            }

            return a;
        }

        private static void pushMessage(int id)
        {
            var lowId = id - windowSize;
            array[id] = createMessage(id);
            if (lowId >= 0)
            {
                array[lowId] = null;
            }
        }

        static void Main(string[] args)
        {
            var worst = new TimeSpan();
            var sw = new Stopwatch();
            for (var i = 0; i < msgCount; i++)
            {
                sw.Reset();
                sw.Start();
                pushMessage(i);
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