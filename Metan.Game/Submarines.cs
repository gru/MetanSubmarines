using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Runtime.CompilerServices;
using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Graphics;
using Microsoft.Xna.Framework.Input;
using static Metan.Constants;
using static Metan.Functions;

namespace Metan
{
    public sealed class Submarines : Game
    {
        private readonly GraphicsDeviceManager _graphicsDeviceManager;
        private SpriteBatch _spriteBatch;
        
        public Submarines()
        {
            TargetElapsedTime = TimeSpan.FromSeconds(0.1);

            _graphicsDeviceManager = new GraphicsDeviceManager(this);
            
            Options = new GameOptions();
        }
        
        public int Id;

        public GameOptions Options { get; }

        public GameState State { get; private set; }

        public bool Ready => State != null && GraphicsDevice != null;
        
        public event EventHandler<KeyboardEvent> KeyboardEvent;
            
        protected override void Update(GameTime gameTime)
        {
            var keyboardState = Keyboard.GetState();
            if (keyboardState.IsKeyDown(Keys.Up))
                OnKeyboardEvent(UserEvent.MoveDown);
            else if (keyboardState.IsKeyDown(Keys.Down))
                OnKeyboardEvent(UserEvent.MoveUp);
                
            if (keyboardState.IsKeyDown(Keys.Left))
                OnKeyboardEvent(UserEvent.MoveLeft);
            else if (keyboardState.IsKeyDown(Keys.Right))
                OnKeyboardEvent(UserEvent.MoveRight);
            
            if (keyboardState.IsKeyDown(Keys.A))
                OnKeyboardEvent(UserEvent.FireLeft);
            else if (keyboardState.IsKeyDown(Keys.D))
                OnKeyboardEvent(UserEvent.FireRight);
                
            if (keyboardState.IsKeyDown(Keys.W))
                OnKeyboardEvent(UserEvent.FireDown);
            else if (keyboardState.IsKeyDown(Keys.S))
                OnKeyboardEvent(UserEvent.FireUp);
            
            if (keyboardState.IsKeyDown(Keys.H))
                OnKeyboardEvent(Options.ShowHitBox 
                    ? UserEvent.HideHitBox 
                    : UserEvent.ShowHitBox);

            if (keyboardState.IsKeyDown(Keys.Escape))
                OnKeyboardEvent(UserEvent.Leave);
            
            base.Update(gameTime);
        }

        protected override void Draw(GameTime gameTime)
        {
            base.Draw(gameTime);

            GraphicsDevice?.Clear(Color.CornflowerBlue);

            _spriteBatch.Begin();
            
            foreach (var (_, vehicle) in State.Vehicles)
            {
                _spriteBatch.Draw(
                    vehicle.Texture, 
                    new Rectangle(vehicle.HitBox.TopLeft, vehicle.HitBox.Size),
                    Color.Chocolate);
            }

            foreach (var (_, crate) in State.Crates)
            {
                _spriteBatch.Draw(
                    crate.Texture,
                    new Rectangle(crate.HitBox.TopLeft, crate.HitBox.Size),
                    Color.Aquamarine);
            }

            foreach (var (_, bullet) in State.Bullets)
            {
                var size = new Point(6, 6);
                var topLeft = new Point(
                    bullet.HitBox.TopLeft.X + 2, bullet.HitBox.TopLeft.Y + 2);
                
                _spriteBatch.Draw(
                    bullet.Texture,
                    new Rectangle(topLeft, size),
                    Color.Black);
            }
            
            _spriteBatch.End();
        }
        
        protected override void LoadContent()
        {
            _spriteBatch = new SpriteBatch(GraphicsDevice);

            State = new GameState(new GameResources(GraphicsDevice));
        }

        protected override void UnloadContent()
        {
            _spriteBatch.Dispose();
        }
        
        protected override void Dispose(bool disposing)
        {
            _spriteBatch.Dispose();
            
            State.Dispose();
            
            base.Dispose(disposing);
        }
        
        private void OnKeyboardEvent(UserEvent evt)
        {
            Console.WriteLine(evt);
            
            var handler = KeyboardEvent;
            if (handler != null)
                handler(this, new KeyboardEvent(evt));
        }
    }

    public sealed class KeyboardEvent : EventArgs
    {
        public KeyboardEvent(UserEvent evt)
        {
            Event = evt;
        }
        
        public UserEvent Event { get; }
    }

    public enum UserEvent : byte
    {
        MoveUp, 
        MoveDown, 
        MoveLeft, 
        MoveRight, 
        FireUp,
        FireDown,
        FireLeft,
        FireRight,
        SpawnBot,
        ShowHitBox,
        HideHitBox,
        Leave
    }

    public readonly struct ClientHitBox
    {
        public ClientHitBox(int tlx, int tly, int brx, int bry)
            : this(new Point(tlx, tly), new Point(brx, bry))
        {
        }
        
        public ClientHitBox(Point tl, Point br)
        {
            TopLeft = ToGamePoint(tl);
            BottomRight = ToGamePoint(br);
            Size = ToGameSize(br, tl);
        }
        
        public readonly Point TopLeft;
        public readonly Point BottomRight;
        public readonly Point Size;
        
        public (Point tl, Point br) Deconstruct() 
            => (TopLeft, BottomRight);
    }

    public record ClientVehicle(int Id, ClientHitBox HitBox, int Damage, Texture2D Texture);
    public record ClientCrate(int Id, ClientHitBox HitBox, Texture2D Texture);
    public record ClientBullet(int Id, ClientHitBox HitBox, Texture2D Texture);
    
    public static class Constants
    {
        public const int WorldCellSize = 10;
    }

    public static class Functions
    {
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static Point ToGamePoint(Point point)
            => new Point(point.X * WorldCellSize, point.Y * WorldCellSize);
        
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static Point ToWorldPoint(Point point)
            => new Point(point.X / WorldCellSize, point.Y / WorldCellSize);
        
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static Point ToGameSize(Point br, Point tl)
            => new Point((br.X - tl.X + 1) * WorldCellSize, (br.Y - tl.Y + 1) * WorldCellSize);
    }

    public class GameState : IDisposable
    {
        private readonly GameResources _resources;

        internal GameState(GameResources resources)
        {
            _resources = resources;
        }

        public Dictionary<int, ClientVehicle> Vehicles { get; }
            = new Dictionary<int, ClientVehicle>();

        public Dictionary<int, ClientCrate> Crates { get; }
            = new Dictionary<int, ClientCrate>();

        public Dictionary<int, ClientBullet> Bullets { get; }
            = new Dictionary<int, ClientBullet>();

        public bool Initialized { get; set; }
        
        public void VehicleAdded(int id, ClientHitBox hitBox, int damage) 
            => Vehicles[id] = new ClientVehicle(id, hitBox, damage, _resources.VehicleTexture);

        public void VehicleRemoved(int id)
            => Vehicles.Remove(id);

        public void VehicleMoved(int id, ClientHitBox hitBox)
        {
            using (new LapTime())
                Vehicles.Set(id, vehicle => vehicle with { HitBox = hitBox });
        } 
        public void CrateAdded(int id, ClientHitBox hitBox)
            => Crates[id] = new ClientCrate(id, hitBox, _resources.CrateTexture);

        public void CrateRemoved(int id) 
            => Crates.Remove(id);

        public void BulletAdded(int id, ClientHitBox hitBox)
        {
            using (new LapTime()) Bullets[id] = new ClientBullet(id, hitBox, _resources.BulletTexture);
        }

        public void BulletRemoved(int id)
            => Bullets.Remove(id);

        public void BulletMoved(int id, ClientHitBox hitBox)
            => Bullets.Set(id, bullet => bullet with { HitBox = hitBox });

        public void Dispose()
        {
            _resources.Dispose();
            
            GC.SuppressFinalize(this);
        }
    }

    internal class GameResources : IDisposable
    {
        private readonly GraphicsDevice _graphicsDevice;

        public GameResources(GraphicsDevice graphicsDevice)
        {
            _graphicsDevice = graphicsDevice;

            BulletTexture = CreateTexture();
            VehicleTexture = CreateTexture();
            CrateTexture = CreateTexture();
        }
        
        public Texture2D BulletTexture { get; }

        public Texture2D VehicleTexture { get; }

        public Texture2D CrateTexture { get; }

        private Texture2D CreateTexture()
        {
            var texture2D = new Texture2D(_graphicsDevice, 1, 1);
            texture2D.SetData(new[] { Color.White });
            return texture2D;
        }
        
        public void Dispose()
        {
            BulletTexture?.Dispose();
            VehicleTexture?.Dispose();
            CrateTexture?.Dispose();
        }
    }
    
    internal static class DictionaryEx
    {
        public static TValue Peek<TKey, TValue>(this Dictionary<TKey, TValue> dictionary, TKey key)
        {
            if (dictionary.TryGetValue(key, out var value))
                dictionary.Remove(key);

            return value;
        }
        
        public static void Set<TKey, TValue>(this Dictionary<TKey, TValue> dictionary, TKey key, Func<TValue, TValue> update)
        {
            if (dictionary.TryGetValue(key, out var value))
                dictionary[key] = update(value);
        }

        public static TValue[] PeekAll<TKey, TValue>(this Dictionary<TKey, TValue> dictionary)
        {
            var result = dictionary.Values.ToArray();

            dictionary.Clear();
            
            return result;
        }
    }

    public class GameOptions
    {
        public bool ShowHitBox { get; set; }
    }

    internal class LapTime : IDisposable
    {
        private readonly Stopwatch _sw;

        public LapTime()
        {
            _sw = Stopwatch.StartNew();
        }
        
        public void Dispose()
        {
            _sw.Stop();
            
            Console.WriteLine(_sw.ElapsedMilliseconds);
        }
    }
}