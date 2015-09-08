using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Xml.Linq;

namespace splitter
{
    public class Fragment
    {
        public string Id { get; set; }

        public XElement Content { get; set; }
    }

    class Program
    {
        //<VTM>
        // <VTMID>68088000</VTMID>
        // <NM>Acebutolol</NM>
        //</VTM>

        //<VTM>
        // <VTMID>13868411000001104</VTMID>
        // <NM>Promethazine hydrochloride</NM>
        // <VTMIDPREV>404843004</VTMIDPREV>
        // <VTMIDDT>2008-08-08</VTMIDDT>
        //</VTM>

        static void Main(string[] args)
        {
            if (args.Length < 2)
                throw new Exception("must specify input file and output directory");

            var filename = args[0];
            var outputdir = args[1];
            if (!Directory.Exists(outputdir))
                Directory.CreateDirectory(outputdir);

            var doc = XDocument.Load(filename);

            var fragments = ProcessWithId(doc.Root);

            foreach (var fragment in fragments)
            {
                var path = Path.Combine(outputdir, fragment.Id + ".xml");
                using (var stream = new StreamWriter(path))
                    stream.Write(fragment.Content.ToString());
            }
        }

        static IEnumerable<Fragment> ProcessWithId(XElement element)
        {
            var childFragments = FindAllAddressableChildren(element)
                .SelectMany(ProcessWithId)
                .Concat(new List<Fragment>{new Fragment
            {
                Id = DeriveId(element),
                Content = CreateLinks(element)
            }});

            return childFragments;
        }

        public static string DeriveId(XElement element)
        {
            return element.Attribute("id") == null ? "no-id" : element.Attribute("id").Value;
        }

        static IEnumerable<XElement> FindAllAddressableChildren(XContainer element)
        {
            return element.Elements()
                    .SelectMany(e => e.Attribute("id") != null && IsUnitOfWork(e) ?
                        new List<XElement> { e } :
                        FindAllAddressableChildren(e));
        }

        static XElement CreateLinks(XElement element)
        {
            var copy = new XElement(element);
            var addressableChildren = FindAllAddressableChildren(copy).ToList();
            foreach (var addressableChild in addressableChildren)
            {
                var title = addressableChild.Descendants("title").Select(e => e.Value).FirstOrDefault() ?? "";
                addressableChild.ReplaceWith(new XElement("xref", title,
                    new XAttribute("href", DeriveId(addressableChild) + ".xml"),
                    new XAttribute("rel",
                        addressableChild.Attribute("outputclass") != null
                            ? addressableChild.Attribute("outputclass").Value
                            : "")));
            }
            foreach (var xref in copy.Descendants("xref").Where(l => l.Attribute("href").Value.StartsWith("#")).ToList())
                xref.SetAttributeValue("href", xref.Attribute("href").Value.Replace("#", "") + ".xml");

            return copy;
        }

        static readonly List<string> TypesOfInterest = new List<string>
        {
            "publication",
			"evidenceCategories",

			"borderlineSubstance",
			"borderlineSubstanceAcbs",
			"borderlineSubstanceTaxonomy",
			"borderlineSubstancePrep",

			"woundManagement",
			"productGroups",

			"treatmentSummary",

			"drugClassifications",
			"drug",
			"drugClass",
			"medicalDevice",
			"medicalDeviceType",
			"medicinalForm",

			"#clinicalMedicinalProductInformation",

			"interaction",

            //no output class
			"#PHP101868",
			"#PHP101869",

            //the lists
            "#drugs",
            "#treatmentSummaries",
            "#medicalDevices",
            "#interactions",
            "#guidance",
            "#about",
            "#borderlineSubstances",
            "#drugClasses",
            "#labels"
        };


        static bool IsUnitOfWork(XElement xElement)
        {
            if (xElement.Name != "topic") return false;

            var type = GetTopicType(xElement);

            return TypesOfInterest.Contains(type);
        }

        static string GetTopicType(XElement topic)
        {
            if (!topic.HasAttributes) return "";

            var id = topic.GetAttributeValue("id").Trim();
            var outputclass = topic.GetAttributeValue("outputclass").Trim();

            if ((id == "" || id.StartsWith("PHP") || id.StartsWith("bnf_")) && outputclass != "")
            {
                var types = outputclass.Contains(" ") ? outputclass.Split(new[] { ' ' }) : new[] { outputclass };
                if (types.Length > 1 && new[] { "about", "guidance" }.Contains(types[1]))
                    types = types.Reverse().ToArray();

                return types[0];
            }
            if (id.StartsWith("bnf_"))
            {
                return "drugInterction";
            }
            return "#" + id;
        }
    }

    public static class XElementExtensions
    {
        public static string GetAttributeValue(this XElement element, string name, string defaultValue = "")
        {
            var attibute = element.Attribute(name);
            return attibute == null ? defaultValue : attibute.Value;
        }
    }
}
