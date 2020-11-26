using System;
using System.Diagnostics;

namespace dotnet
{
    public class Message
    {
        public byte[] Content;

        public Message(int size)
        {
            Content = new byte[size];
        }
    }

    public static class Program
    {
        const int windowSize = 200000;
        const int messageCount = 1000000;

        private static TimeSpan worst = TimeSpan.Zero;
        private static Stopwatch timer = new Stopwatch();
        private static Message[] buffer = new Message[windowSize];

        public static void Main(string[] args)
        {
            for (int i = 0; i < messageCount; i++)
            {
                PushMessage(i);
            }

            Console.WriteLine($"Worst push time: {worst}");
        }

        private static void PushMessage(int index)
        {
            timer.Restart();
            var message = CreateMessage();
            buffer[index % windowSize] = message;
            var elapsed = timer.Elapsed;
            if (elapsed > worst)
            {
                worst = elapsed;
            }
        }

        private static Message CreateMessage()
            => new Message(1024);
    }
}
