using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Runtime.CompilerServices;
using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Content;
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
            _graphicsDeviceManager = new GraphicsDeviceManager(this);
            
            Options = new GameOptions();
        }
        
        public int Id;

        public GameOptions Options { get; }

        public GameState State { get; private set; }
        
        public GameResources Resources { get; private set; }
        
        public bool Ready => 
            GraphicsDevice != null &&
            Resources != null && 
            State != null;
        
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
            
            if (keyboardState.IsKeyDown(Keys.B))
                OnKeyboardEvent(UserEvent.SpawnBot);

            base.Update(gameTime);
        }

        protected override void Draw(GameTime gameTime)
        {
            base.Draw(gameTime);

            GraphicsDevice?.Clear(Color.CornflowerBlue);

            _spriteBatch.Begin();
            
            var delta = gameTime.ElapsedGameTime.TotalMilliseconds;
            
            foreach (var (_, vehicle) in State.Vehicles)
            {
                var current = vehicle.Position;
                var desired = vehicle.HitBox.TopLeft;

                vehicle.Position = Move(current, desired, delta);
                
                _spriteBatch.Draw(
                    Resources.GetVehicleTexture(vehicle.CellSize), 
                    new Rectangle(vehicle.Position, vehicle.HitBox.Size),
                    Color.Chocolate);
            }

            foreach (var (_, crate) in State.Crates)
            {
                _spriteBatch.Draw(
                    Resources.CratesTexture,
                    new Rectangle(crate.HitBox.TopLeft, crate.HitBox.Size),
                    Resources.GetCrateSprite(crate.Bonus),
                    Color.Aqua);
            }

            foreach (var (_, bullet) in State.Bullets)
            {
                var current = bullet.Position;
                var desired = bullet.HitBox.TopLeft;

                bullet.Position = Move(current, desired, delta * bullet.Speed);
                
                var size = new Point(BulletSize, BulletSize);
                var topLeft = new Point(
                    bullet.Position.X + BulletBorder, 
                    bullet.Position.Y + BulletBorder);
                
                _spriteBatch.Draw(
                    Resources.BulletTexture,
                    new Rectangle(topLeft, size),
                    Color.Black);
            }
            
            _spriteBatch.End();
        }
        
        protected override void LoadContent()
        {
            _spriteBatch = new SpriteBatch(GraphicsDevice);

            State = new GameState();
            Resources = new GameResources(Content, GraphicsDevice);
        }

        protected override void UnloadContent()
        {
            _spriteBatch.Dispose();
        }
        
        protected override void Dispose(bool disposing)
        {
            _spriteBatch.Dispose();
            
            base.Dispose(disposing);
        }
        
        private void OnKeyboardEvent(UserEvent evt)
        {
            if (!State.SentCommands.Add(evt)) 
                return;

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

        public override string ToString()
        {
            return $"TL:{TopLeft};BR:{BottomRight};S:{Size}";
        }
    }

    public class ClientVehicle
    {
        public ClientVehicle(int id, Point position, ClientHitBox hitBox, int damage, VehicleCellSize cellSize)
        {
            Id = id;
            Position = position;
            HitBox = hitBox;
            Damage = damage;
            CellSize = cellSize;
        }

        public int Id { get; }
        public Point Position { get; set; }
        public ClientHitBox HitBox { get; set; }
        public int Damage { get; set; }
        public VehicleCellSize CellSize { get; set; }
    }

    public class ClientCrate
    {
        public ClientCrate(int id, ClientHitBox hitBox, CrateBonus bonus)
        {
            Id = id;
            HitBox = hitBox;
            Bonus = bonus;
        }

        public int Id { get; set; }
        public ClientHitBox HitBox { get; set; }
        public CrateBonus Bonus { get; set; }
    }

    public class ClientBullet
    {
        public ClientBullet(int id, Point position, ClientHitBox hitBox, int speed)
        {
            Id = id;
            Position = position;
            Speed = speed;
            HitBox = hitBox;
        }

        public int Id { get; set; }
        public Point Position { get; set; }
        public ClientHitBox HitBox { get; set; }
        public int Speed { get; set; }        
    }

    public enum CrateBonus
    {
        HealthBonus,
        DamageBonus,
        ShapeBonus,
        RandomBonus
    }

    public enum VehicleCellSize
    {
        One, 
        Two,
        Three,
        Four
    }
    
    public static class Constants
    {
        public const int WorldCellSize = 10;
        public const int BulletSize = 2;
        public const int BulletBorder = (WorldCellSize - BulletSize) / 2;
        public const double SpeedFactor = 0.15;
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
        
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static Point Move(Point current, Point desired, double delta)
        {
            return new Point(
                Move(current.X, desired.X, delta),
                Move(current.Y, desired.Y, delta));
        }

        private static int Move(int current, int desired, double delta)
        {
            if (current == desired)
                return desired;
            
            var diffX = Math.Abs(current - desired);
            var value = (int)(SpeedFactor * delta);
            if (value > diffX)
            {
                current = desired;
            }
            else
            {
                if (current < desired)
                    current += value;
                else
                    current -= value;    
            }

            return current;
        }
    }

    public class GameState
    {
        public HashSet<UserEvent> SentCommands { get; }
            = new HashSet<UserEvent>();

        public Dictionary<int, ClientVehicle> Vehicles { get; }
            = new Dictionary<int, ClientVehicle>();

        public Dictionary<int, ClientCrate> Crates { get; }
            = new Dictionary<int, ClientCrate>();

        public Dictionary<int, ClientBullet> Bullets { get; }
            = new Dictionary<int, ClientBullet>();

        public bool Initialized { get; set; }
        
        public void VehicleAdded(int id, ClientHitBox hitBox, int damage, VehicleCellSize cellSize) 
            => Vehicles[id] = new ClientVehicle(id, hitBox.TopLeft, hitBox, damage, cellSize);

        public void VehicleRemoved(int id)
            => Vehicles.Remove(id);

        public void VehicleMoved(int id, ClientHitBox hitBox)
            => Vehicles.Set(id, v => v.HitBox = hitBox);

        public void VehicleShaped(int id, ClientHitBox hitBox, VehicleCellSize cellSize)
            => Vehicles.Set(id, v => { v.HitBox = hitBox; v.CellSize = cellSize; });

        public void CrateAdded(int id, ClientHitBox hitBox, CrateBonus bonus)
            => Crates[id] = new ClientCrate(id, hitBox, bonus);

        public void CrateRemoved(int id) 
            => Crates.Remove(id);

        public void BulletAdded(int id, ClientHitBox hitBox, int speed) 
            => Bullets[id] = new ClientBullet(id, hitBox.TopLeft, hitBox, speed);

        public void BulletRemoved(int id)
            => Bullets.Remove(id);

        public void BulletMoved(int id, ClientHitBox hitBox)
            => Bullets.Set(id, bullet => bullet.HitBox = hitBox);

        public void Tick() => SentCommands.Clear();
    }

    public class GameResources : IDisposable
    {
        private readonly ContentManager _contentManager;
        private readonly GraphicsDevice _graphicsDevice;
        
        private Texture2D _cratesTexture;
        private Texture2D _submarineOneCellTexture;
        private Texture2D _submarineTwoCellTexture;
        private Texture2D _submarineThreeCellTexture;
        private Texture2D _submarineFourCellTexture;

        public GameResources(ContentManager contentManager, GraphicsDevice graphicsDevice)
        {
            _contentManager = contentManager;
            _graphicsDevice = graphicsDevice;

            BulletTexture = CreateTexture();
        }
        
        public Texture2D BulletTexture { get; }

        public Texture2D SubmarineOneCellTexture 
            => _submarineOneCellTexture ??= GetImageResource("submarine_1");
        public Texture2D SubmarineTwoCellTexture 
            => _submarineTwoCellTexture ??= GetImageResource("submarine_2");
        public Texture2D SubmarineThreeCellTexture 
            => _submarineThreeCellTexture ??= GetImageResource("submarine_3");
        public Texture2D SubmarineFourCellTexture 
            => _submarineFourCellTexture ??= GetImageResource("submarine_4");
        public Texture2D CratesTexture 
            => _cratesTexture ??= GetImageResource("crates");

        public Rectangle GetCrateSprite(CrateBonus bonus)
        {
            return bonus switch
            {
                CrateBonus.HealthBonus => new Rectangle(0, 0, 10, 10),
                CrateBonus.DamageBonus => new Rectangle(10, 0, 10, 10),
                CrateBonus.ShapeBonus => new Rectangle(20, 0, 10, 10),
                CrateBonus.RandomBonus => new Rectangle(30, 0, 10, 10),
                _ => throw new ArgumentOutOfRangeException(nameof(bonus), bonus, null)
            };
        }

        public Texture2D GetVehicleTexture(VehicleCellSize size)
        {
            return size switch
            {
                VehicleCellSize.One => SubmarineOneCellTexture,
                VehicleCellSize.Two => SubmarineTwoCellTexture,
                VehicleCellSize.Three => SubmarineThreeCellTexture,
                VehicleCellSize.Four => SubmarineFourCellTexture,
                _ => throw new ArgumentOutOfRangeException(nameof(size), size, null)
            };
        }
        
        private Texture2D CreateTexture()
        {
            var texture2D = new Texture2D(_graphicsDevice, 1, 1);
            texture2D.SetData(new[] { Color.White });
            return texture2D;
        }

        private Texture2D GetImageResource(string name)
            => _contentManager.Load<Texture2D>($"Content/images/{name}");
        
        public void Dispose()
        {
            BulletTexture?.Dispose();
            SubmarineOneCellTexture?.Dispose();
        }
    }
    
    public class GameOptions
    {
        public bool ShowHitBox { get; set; }
    }
    
    internal static class DictionaryEx
    {
        public static void Set<TKey, TValue>(this Dictionary<TKey, TValue> dictionary, TKey key, Action<TValue> update)
        {
            if (dictionary.TryGetValue(key, out var value)) update(value);
        }
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