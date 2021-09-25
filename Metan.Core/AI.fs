namespace Metan.Core

open System

type BotState =
    | SearchCrate
    | MoveTo of Position * Position
    | Idle of Time

module AI =
    exception InvalidState
    
    let private findClosestCrate cs hb =
        cs |> List.filter (fun c -> HitBox.intersect c.hitBox hb)
           |> List.tryHead
        
    let private contains pos = function
        | Projection sl ->
            sl |> List.map Reflection.getPos |> List.contains pos
        
    let searchCrate rnd game (me:Vehicle) =
        let cr = me.hitBox
                 |> HitBox.expand 10
                 |> HitBox.crop game.size
                 |> findClosestCrate game.crates
        let pos =
            match cr with
            | Some c -> HitBox.topLeft c.hitBox
            | None -> Position.getRandom rnd game.size
        MoveTo(pos, (0, 0)), []
        
    let move rnd game (me:Vehicle) = function
        | MoveTo(pos, prev) ->
            if (HitBox.topLeft me.hitBox) = prev
            then
                MoveTo(Position.getRandom rnd game.size, (0, 0)), []
            else
                let pr = Projection.project me.hitBox me.shape
                if pr |> contains pos then
                    Idle (game.time + 20u), []
                else
                    let tl = Projection.topLeft pr
                    let lx = (fst tl) - (fst pos)
                    let ly = (snd tl) - (snd pos)
                    let dx =
                        if abs lx > abs ly then
                            if lx > 0
                            then Left
                            else Right
                        else
                            if ly < 0
                            then Up
                            else Down
                    (MoveTo (pos, HitBox.topLeft me.hitBox)), [AreaCommand(Move(me.id, dx))]
        | _ -> raise InvalidState;
        
    let idle game = function
        | Idle till ->
            if game.time = till
            then SearchCrate, []
            else Idle till, []
        | _ -> raise InvalidState