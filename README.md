# Import style GHC plugin

The plugin helps to unify imports style via compile warnings/errors

Tested with:
  * 9.2.7
  * 9.4.8
  * 9.6.4
  * 9.8.2

## Usage

### Yaml config
#### Enable plugin in your cabal file:
```cabal
  build-depends: ..., import-style-plugin, ...

  ghc-options: -fplugin ImportStylePlugin.Yaml -fplugin-opt ImportStylePlugin.Yaml:<path-to-config>
```
#### Write config:

```yaml
qualificationStyle: Post | Pre | null

bannedModules:
  Module.Name:
    severity: Error | Warning
    why: string with ban reason

importRules:
  DataModuleName:
    severity: Error | Warning
    rules:
      - qualification: Required | Forbidden | null
        aliases:
          # Either `exactly`, so alias can't be omitted 
          exactly:
            - Module.Alias
            - Alias
          # or just list, so alias can be omitted
          - Module.Alias
          - Alias
        importedNames:
          # Either whitelist, so import should be 
          # accompanied with explicit import list 
          whitelist:
            - pattern (:|>) 
            - type (+)
            - foo
          # or blacklist, so import should be 
          # accompanied with explicit hiding list with all these names
          # or explicit import list shouldn't contain any of these names 
          blacklist:
            - pattern (:|>) 
            - type (+)
            - foo
```
#### Here is example:
```yaml
qualificationStyle: Post

bannedModules:
  # Forbids Data.Map at all
  Data.Map:
    severity: Error
    why: Use 'Data.Map.Strict'

importRules:
  # Either
  # import Data.Text (Text)
  # or
  # import Data.Text qualified as Text
  Data.Text:
    severity: Warning
    rules:
      - qualification: Required
        aliases:
          exactly: 
            - Text

      - qualification: Forbidden
        importedNames:
          whitelist: 
            - Text
```
### Derived plugin
It can be useful if you have a lot of projects with one style. So you haven't to copy paste config but just use _style_ as library

  1. Create library package
  2. Add to deps `import-style-plugin`
  3. Create module `YourPlugin.hs` (or something like that, it's unimportant)
  4. Put there 
      ```haskell
      module YourPlugin where
      
      import ImportStylePlugin.Derived qualified as Derived
      import ImportStylePlugin.Config as Cfg

      -- variable name `plugin` IS important
      -- Configure your style with Haskell types from `ImportStylePlugin.Config`
      plugin = Derived.plugin
        ImportsStyle
          { qualificationStyle = ...
          , bannedModules = ...
          , importRules = ... 
          }
      ```
  5. Plug in
      ```cabal
        build-depends: ..., your-import-style-plugin, ...

        ghc-options: -fplugin YourPlugin
      ```
## Tips
### Custom warning
Since ghc-9.8 the plugin introduces custom warning `x-import-style`. So if you set `-Werror` but still want to style warnings be warnings use ghc option `-Wwarn=x-import-style`. You also can disable at all warnings with `-Wno-x-import-style`