namespace Metan.Core

open System

type BotState =
    | SearchCrate
    | MoveTo of Position * Position * uint
    | Idle of Time

module AI =
    let private findById id game =
        game.vehicles |> List.find (fun v -> v.id = id)
    
    let private findClosestCrate cs hb =
        cs |> List.filter (fun c -> HitBox.intersect c.hitBox hb)
           |> List.tryHead
        
    let private contains pos = function
        | Projection sl ->
            sl |> List.map Reflection.getPos |> List.contains pos
        
    let searchCrate rnd game id =
        let me = findById id game
        let cr = me.hitBox
                 |> HitBox.expand 10
                 |> HitBox.crop game.size
                 |> findClosestCrate game.crates
        let pos =
            match cr with
            | Some c -> HitBox.topLeft c.hitBox
            | None -> Position.getRandom rnd game.size
        MoveTo(pos, HitBox.topLeft me.hitBox, 0u), []
        
    let move game id pos =
        let me = findById id game
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
            (MoveTo (pos, HitBox.topLeft me.hitBox, 0u)), [AreaCommand(Move(me.id, dx))]
        
    let idle game till =
        if game.time = till
        then SearchCrate, []
        else Idle till, []