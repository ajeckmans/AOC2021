namespace Puzzles
open System.Reflection

module inputs =
    type marker = interface end

    let GetResourceStream name =
        let assembly = Assembly.GetAssembly(typeof<marker>)
        assembly.GetManifestResourceStream("Puzzles.inputs." + name)