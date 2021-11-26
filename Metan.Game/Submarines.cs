using System;
using System.Collections.Generic;
using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Graphics;
using Microsoft.Xna.Framework.Input;

namespace Metan
{
    public sealed class Submarines : Game
    {
        private readonly GraphicsDeviceManager _graphicsDeviceManager;
        private readonly Dictionary<int, ClientVehicle> _vehicles;
        private SpriteBatch _spriteBatch;
        
        public Submarines()
        {
            _graphicsDeviceManager = new GraphicsDeviceManager(this);
           
            _vehicles = new Dictionary<int, ClientVehicle>(10);
            
            
        }
        
        public int Id;
        public bool ShowHitBox; 
        public event EventHandler<KeyboardEvent> KeyboardEvent;

        public bool ContainsVehicle(int id) 
            => _vehicles.ContainsKey(id);

        public void AddVehicle(int id, ClientHitBox hitBox, int damage)
        {
            if (GraphicsDevice == null) return;
            var texture2D = new Texture2D(GraphicsDevice, 1, 1);
            texture2D.SetData(new[] { Color.White });
            
            _vehicles[id] = new ClientVehicle(id, hitBox, damage, texture2D);
        }

        public void MoveVehicle(int id, ClientHitBox hb)
        {
            if (_vehicles.TryGetValue(id, out var vehicle))
                _vehicles[id] = vehicle with { HitBox = hb };
        }
        
        public void Clear()
        {
            //GraphicsDevice?.Clear(Color.CornflowerBlue);
        }
        
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
                OnKeyboardEvent(UserEvent.FireUp);
            else if (keyboardState.IsKeyDown(Keys.S))
                OnKeyboardEvent(UserEvent.FireDown);
            
            if (keyboardState.IsKeyDown(Keys.H))
                OnKeyboardEvent(ShowHitBox 
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
            
            foreach (var (_, vehicle) in _vehicles)
            {
                _spriteBatch.Draw(
                    vehicle.Texture2D, 
                    new Rectangle(vehicle.HitBox.TopLeft, vehicle.HitBox.Size),
                    Color.Chocolate);
            }
            
            _spriteBatch.End();

        }
        
        protected override void LoadContent()
        {
            _spriteBatch = new SpriteBatch(GraphicsDevice);
        }

        protected override void UnloadContent()
        {
            _spriteBatch.Dispose();
        }
        
        protected override void Dispose(bool disposing)
        {
            _spriteBatch.Dispose();
            
            foreach (var (_, vehicle) in _vehicles)
                vehicle.Dispose();
            
            base.Dispose(disposing);
        }
        
        private void OnKeyboardEvent(UserEvent evt)
        {
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
            TopLeft = tl;
            BottomRight = br;
            Size = new Point(br.X - tl.X + 11, br.Y - tl.Y + 11);
        }
        
        public readonly Point TopLeft;
        public readonly Point BottomRight;
        public readonly Point Size;
        public (Point tl, Point br) Deconstruct() 
            => (TopLeft, BottomRight);
    }

    public record ClientVehicle(int Id, ClientHitBox HitBox, int Damage, Texture2D Texture2D) : IDisposable
    {
        public void Dispose()
        {
            Texture2D?.Dispose();
        }
    }
}